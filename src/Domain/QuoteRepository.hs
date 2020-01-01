{-# LANGUAGE OverloadedStrings #-}

module Domain.QuoteRepository ( QuoteRepository
                             , inMemoryQuoteRepo
                             , fileBasedQuoteRepository
                             , getAll
                             , save
                             ) where

import Domain.Quote as Quote
import Control.Concurrent.STM
import Data.UUID
import Network.URL (URL, importURL, exportURL)
import Data.Maybe (fromJust)
import qualified System.Directory as D
import qualified Data.Text as T
import qualified Data.Map as M
import Data.RDF
import Text.RDF.RDF4H.TurtleSerializer
import System.IO (withFile, IOMode(WriteMode))
import System.FilePath

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

quoteToRdfGraph :: FilePath -> Quote -> RDF TList
quoteToRdfGraph uri quote = let myEmptyGraph = empty :: RDF TList
                                typeTriple = triple
                                  (unode (T.pack uri))
                                  (unode "rdf:type")
                                  (unode "schema:Quotation")
                                textTriple = triple
                                  (unode (T.pack uri))
                                  (unode "schema:text")
                                  (LNode (PlainL $ quoteText quote))
                                creatorTriple = triple
                                  (unode (T.pack uri))
                                  (unode "schema:spokenByCharacter")
                                  (unode (quoteeURIText (saidBy quote)))
                                g1 = addTriple myEmptyGraph creatorTriple
                                g2 = addTriple g1 textTriple
                            in
                              addTriple g2 typeTriple

fileBasedQuoteRepository :: IO QuoteRepository
fileBasedQuoteRepository = do
  dir <- quoteDir
  let mappings = PrefixMappings $ M.fromList [("schema", "http://schema.org/")]
  pure $ Repo { getById = \_ -> return Nothing
              , getAll = pure []
              , save = \quote -> do
                  let quoteUri = "/quote/" ++ (show $ getId quote)
                      graph = quoteToRdfGraph quoteUri quote
                      docUrl = T.concat ["localhost:3000/quote/", T.pack $ show $ getId quote]
                      serializer = TurtleSerializer (Just docUrl) mappings
                      quoteFilePath = quoteToPath dir quote
                  withFile quoteFilePath WriteMode (\handle -> hWriteRdf serializer handle graph)
              }
