{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Domain.QuoteRepository ( Repository(..)
                              , QuoteRepository
                              , linkedDataQuoteRepository
                              , ldQuoteContainer
                              , getQuoteUrls
                              , getAllQuotes
                              ) where

import Domain.Quote as Quote
import Data.UUID (UUID, fromString)
import Network.URL (URL, importURL, exportURL)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.RDF
import qualified DontQuoteMe.Data.RDF as QRDF
import System.IO (withFile, IOMode(WriteMode))
import Network.HTTP.Simple as HTTP
import qualified System.IO.Temp as Temp

type ID = UUID

data Repository i a = Repo { getById :: i -> IO (Maybe a)
                           , getAll :: IO [a]
                           , save :: a -> IO ()
                           }

type QuoteRepository = Repository ID Quote

quoteToUrl :: Quote -> URL
quoteToUrl quote = fromJust $ importURL $ "localhost:3000/quote/" ++ show (getId quote)

getQuoteUrls :: URL      -- ^base address of Linked Data server
             -> IO [URL] -- ^list of URLs to quotes
getQuoteUrls base = do
  let url = exportURL base
      tp = TurtleParser Nothing Nothing
  eitherRDF <- parseURL tp url
  case eitherRDF of
    Left _ -> pure []
    Right (graph :: RDF TList) -> let quoteTriples = query graph Nothing (Just $ QRDF.mkPrefixedNode QRDF.ldpPrefix "contains") Nothing
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
      QRDF.rdfToQuote id' graph

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
  let graph = QRDF.quoteToRdfGraph quote
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
