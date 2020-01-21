{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Domain.QuoteRepository ( Repository(..)
                              , QuoteRepository
                             , fileBasedQuoteRepository
                             , linkedDataQuoteRepository
                             , ldQuoteContainer
                             , getQuoteUrls
                             , getAllQuotes
                             , quoteToRdfGraph
                             ) where

import Domain.Quote as Quote
import Data.UUID (UUID, fromString)
import Network.URL (URL, importURL, exportURL)
import Data.Maybe (fromJust, fromMaybe, catMaybes, listToMaybe)
import qualified System.Directory as D
import qualified Data.Text as T
import qualified Data.Map as M
import Data.RDF
import Text.RDF.RDF4H.TurtleSerializer
import System.IO (withFile, IOMode(WriteMode), Handle)
import System.FilePath ((</>), splitFileName, splitExtension)
import System.Directory (listDirectory)
import Network.HTTP.Simple as HTTP
import qualified System.IO.Temp as Temp

type ID = UUID

data Repository i a = Repo { getById :: i -> IO (Maybe a)
                           , getAll :: IO [a]
                           , save :: a -> IO ()
                           }

type QuoteRepository = Repository ID Quote

quoteDir :: IO FilePath
quoteDir = do
  dir <- D.getXdgDirectory D.XdgData "dont-quote-me"
  D.createDirectoryIfMissing True dir
  pure dir

quoteToPath :: FilePath -> Quote -> FilePath
quoteToPath dir quote = dir </> (show $ getId quote) ++ ".ttl"

anonymousURI :: URL
anonymousURI = fromJust $ importURL "http://www.wikidata.org/wiki/Q4233718"

quoteeURI :: Quotee -> URL
quoteeURI Anonymous = anonymousURI
quoteeURI (Person url) = url

quoteeURIText :: Quotee -> T.Text
quoteeURIText quotee = T.pack $ exportURL $ quoteeURI quotee

rdfPrefix :: T.Text
rdfPrefix = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

schemaPrefix :: T.Text
schemaPrefix = "http://schema.org/"

ldpPrefix :: T.Text
ldpPrefix = "http://www.w3.org/ns/ldp#"

mkPrefixedNode :: T.Text -> T.Text -> Node
mkPrefixedNode prefix term = (unode (T.concat [prefix, term]))

quoteToRdfGraph :: Quote -> RDF TList
quoteToRdfGraph quote = let myEmptyGraph = empty :: RDF TList
                            quoteNode = unode (T.concat ["http://localhost:3000/quote/", (T.pack . show) (getId quote)])
                            typeTriple = triple
                              quoteNode
                              (mkPrefixedNode rdfPrefix "type")
                              (mkPrefixedNode schemaPrefix "Quotation")
                            textTriple = triple
                              quoteNode
                              (mkPrefixedNode schemaPrefix "text")
                              (LNode (PlainL $ quoteText quote))
                            creatorTriple = triple
                              quoteNode
                              (mkPrefixedNode schemaPrefix "spokenByCharacter")
                              (unode (quoteeURIText (saidBy quote)))
                            g1 = addTriple myEmptyGraph creatorTriple
                            g2 = addTriple g1 textTriple
                            in
                              addTriple g2 typeTriple

nodesToQuote :: UUID -> Node -> Node -> Maybe Quote
nodesToQuote uuid (UNode uriText) (LNode (PlainL quoteText)) = do
  personURI <- importURL (T.unpack uriText)
  pure $ Quote uuid quoteText (Person personURI)
nodesToQuote _ quoteeURINode quoteTextNode = Nothing

rdfToQuote :: UUID -> RDF TList -> Maybe Quote
rdfToQuote uuid rdf = do
  _ <- listToMaybe $ query rdf Nothing (Just (mkPrefixedNode rdfPrefix "type")) (Just (mkPrefixedNode schemaPrefix "Quotation"))
  quoteeTriple <- listToMaybe $ query rdf Nothing (Just (mkPrefixedNode schemaPrefix "spokenByCharacter")) Nothing
  quoteTextTriple <- listToMaybe $ query rdf Nothing (Just (mkPrefixedNode schemaPrefix "text")) Nothing
  let quotee = objectOf quoteeTriple
      quoteText = objectOf quoteTextTriple
  nodesToQuote uuid quotee quoteText

quoteIDFromFile :: FilePath -> Maybe UUID
quoteIDFromFile fp = fromString fileName
  where (_, fileNameWithExt) = splitFileName fp
        (fileName, _) = splitExtension fileNameWithExt

quoteForFile :: FilePath -> FilePath -> IO (Maybe Quote)
quoteForFile dir fileName = do
  let tp = TurtleParser Nothing Nothing
      fp = dir </> fileName
  eitherRDF <- parseFile tp fp
  let maybeQuote = do
        rdf <- either (const Nothing) Just eitherRDF -- use `hush` from Control.Error.Util
        uuid <- quoteIDFromFile fp
        rdfToQuote uuid rdf
  pure maybeQuote

quoteToUrl :: Quote -> URL
quoteToUrl quote = fromJust $ importURL $ "localhost:3000/quote/" ++ show (getId quote)

fileBasedQuoteRepository :: IO QuoteRepository
fileBasedQuoteRepository = do
  dir <- quoteDir
  let mappings = PrefixMappings $ M.fromList [("schema", "http://schema.org/")]
  pure $ Repo { getById = \_ -> return Nothing
              , getAll = do
                  fs <- listDirectory dir
                  quoteMaybes <- traverse (quoteForFile dir) fs
                  pure $ catMaybes quoteMaybes
              , save = \quote -> do
                  let graph = quoteToRdfGraph quote
                      docUrl = T.concat ["localhost:3000/quote/", T.pack $ show $ getId quote]
                      serializer = TurtleSerializer (Just docUrl) mappings
                      quoteFilePath = quoteToPath dir quote
                  withFile quoteFilePath WriteMode (\handle -> hWriteRdf serializer handle graph)
              }

getQuoteUrls :: URL      -- ^base address of Linked Data server
             -> IO [URL] -- ^list of URLs to quotes
getQuoteUrls base = do
  let url = exportURL base
      tp = TurtleParser Nothing Nothing
  eitherRDF <- parseURL tp url
  case eitherRDF of
    Left _ -> pure []
    Right (graph :: RDF TList) -> let quoteTriples = query graph Nothing (Just $ mkPrefixedNode ldpPrefix "contains") Nothing
                                      quoteUNodes = map objectOf quoteTriples
                                      quoteUrls = traverse (\(UNode url) -> importURL (T.unpack url)) quoteUNodes
                                  in
                                    pure $ fromMaybe [] quoteUrls

quoteIdFromUrl :: URL -> Maybe UUID
quoteIdFromUrl _ = fromString "16206cb7-b238-4067-ba06-3bd3f84cbc89"

getQuote :: URL -> IO (Maybe Quote)
getQuote quoteUrl = do
  let url = exportURL quoteUrl
      tp = TurtleParser Nothing Nothing
  eitherRDF <- parseURL tp url
  case eitherRDF of
    Left _ -> pure Nothing
    Right (graph :: RDF TList) -> pure $ do
      id' <- quoteIdFromUrl quoteUrl
      rdfToQuote id' graph

ldQuoteContainer :: URL
ldQuoteContainer = fromJust $ importURL "http://localhost/quotes/"

getAllQuotes :: URL -> IO [Quote]
getAllQuotes baseUrl = do
  quoteUrls <- getQuoteUrls ldQuoteContainer
  maybeQuotes <- mapM getQuote quoteUrls
  let quotes = catMaybes maybeQuotes
  pure quotes

saveQuote :: Quote -> IO ()
saveQuote quote = do
  let graph = quoteToRdfGraph quote
      urlStr = T.pack $ exportURL $ quoteToUrl quote
  tempFilePath <- Temp.emptySystemTempFile "quote.rdf"
  _ <- saveGraph graph urlStr tempFilePath
  sendQuote tempFilePath
  where mkRequest :: FilePath -> HTTP.Request
        mkRequest filePath = setRequestBodyFile filePath
                             $ setRequestHeader "Content-Type" ["text/turtle"]
                             $ setRequestPath "/quotes"
                             $ setRequestHost "localhost"
                             $ setRequestMethod "POST"
                             $ defaultRequest

        sendQuote :: FilePath -> IO ()
        sendQuote filePath = HTTP.httpNoBody req >> pure ()
          where req = mkRequest filePath

        saveGraph :: RDF TList -> T.Text -> FilePath -> IO ()
        saveGraph graph urlStr filePath = do
          let mappings = PrefixMappings M.empty
              serializer = TurtleSerializer (Just urlStr) mappings
          withFile filePath WriteMode (\handle -> hWriteRdf serializer handle graph)

linkedDataQuoteRepository :: QuoteRepository
linkedDataQuoteRepository =
  Repo { getById = \_ -> return Nothing
       , getAll = getAllQuotes ldQuoteContainer
       , save = saveQuote
       }
