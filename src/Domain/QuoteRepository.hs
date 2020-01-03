{-# LANGUAGE OverloadedStrings #-}

module Domain.QuoteRepository ( QuoteRepository
                             , inMemoryQuoteRepo
                             , fileBasedQuoteRepository
                             , getAll
                             , save
                             ) where

import Domain.Quote as Quote
import Control.Concurrent.STM
import Data.UUID (UUID, fromString)
import Network.URL (URL, importURL, exportURL)
import Data.Maybe (fromJust, catMaybes, listToMaybe)
import qualified System.Directory as D
import qualified Data.Text as T
import qualified Data.Map as M
import Data.RDF
import Text.RDF.RDF4H.TurtleSerializer
import System.IO (withFile, IOMode(WriteMode))
import System.FilePath ((</>), splitFileName, splitExtension)
import System.Directory (listDirectory)
import Debug.Trace

type ID = UUID

data Repository i a = Repo { getById :: i -> IO (Maybe a)
                           , getAll :: IO [a]
                           , save :: a -> IO ()
                           }

type QuoteRepository = Repository ID Quote

inMemoryQuoteRepo :: IO QuoteRepository
inMemoryQuoteRepo = do
  quotesTVar <- newTVarIO []
  let repo = Repo { getById = \_ -> return Nothing
                  , getAll = readTVarIO quotesTVar
                  , save = \quote -> atomically $ modifyTVar' quotesTVar (quote:)
                  }
      lennon = Person $ fromJust $ importURL "http://dbpedia.org/data/John_Lennon.ttl"
  (Quote.create "Life is what happens when you're busy making other plans." lennon) >>= save repo
  (Quote.create "It is difficult to soar with eagles when you work with turkeys." Anonymous) >>= save repo
  return repo

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

quoteToRdfGraph :: Quote -> RDF TList
quoteToRdfGraph quote = let myEmptyGraph = empty :: RDF TList
                            quoteNode = unode ""
                            typeTriple = triple
                              quoteNode
                              (unode "rdf:type")
                              (unode "schema:Quotation")
                            textTriple = triple
                              quoteNode
                              (unode "schema:text")
                              (LNode (PlainL $ quoteText quote))
                            creatorTriple = triple
                              quoteNode
                              (unode "schema:spokenByCharacter")
                              (unode (quoteeURIText (saidBy quote)))
                            g1 = addTriple myEmptyGraph creatorTriple
                            g2 = addTriple g1 textTriple
                            in
                              addTriple g2 typeTriple

nodesToQuote :: UUID -> Node -> Node -> Maybe Quote
nodesToQuote uuid (UNode uriText) (LNode (PlainL quoteText)) = do
  personURI <- importURL (T.unpack uriText)
  pure $ Quote uuid quoteText (Person personURI)
nodesToQuote _ quoteeURINode quoteTextNode | trace ("Quotee URI Node: " ++ show quoteeURINode ++ ", Quote text Node: " ++ show quoteTextNode) True = Nothing

rdfToQuote :: UUID -> RDF TList -> Maybe Quote
rdfToQuote uuid rdf = do
  _ <- listToMaybe $ query rdf Nothing (Just (unode "rdf:type")) (Just (unode "schema:Quotation"))
  quoteeTriple <- listToMaybe $ query rdf Nothing (Just (unode "schema:spokenByCharacter")) Nothing
  quoteTextTriple <- listToMaybe $ query rdf Nothing (Just (unode "schema:text")) Nothing
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
      fp' = trace ("Attempting to open quote file: " ++ show fp) fp
  eitherRDF <- parseFile tp fp'
  let eitherRDF' = trace ("Either RDF: " ++ show eitherRDF) eitherRDF
  let maybeQuote = do
        rdf <- either (const Nothing) Just eitherRDF'
        uuid <- quoteIDFromFile fp
        rdfToQuote uuid rdf
  pure maybeQuote

fileBasedQuoteRepository :: IO QuoteRepository
fileBasedQuoteRepository = do
  dir <- quoteDir
  let mappings = PrefixMappings $ M.fromList [("schema", "http://schema.org/")]
  pure $ Repo { getById = \_ -> return Nothing
              , getAll = do
                  fs <- listDirectory dir
                  quoteMaybes <- traverse (quoteForFile dir) fs
                  let quoteMaybes' = trace ("Quote Maybes: " ++ show quoteMaybes) quoteMaybes
                  pure $ catMaybes quoteMaybes'
              , save = \quote -> do
                  let graph = quoteToRdfGraph quote
                      docUrl = T.concat ["localhost:3000/quote/", T.pack $ show $ getId quote]
                      serializer = TurtleSerializer (Just docUrl) mappings
                      quoteFilePath = quoteToPath dir quote
                  withFile quoteFilePath WriteMode (\handle -> hWriteRdf serializer handle graph)
              }
