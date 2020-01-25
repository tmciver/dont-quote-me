{-# LANGUAGE OverloadedStrings #-}

module DontQuoteMe.Domain.QuoteRepository.FileBasedQuoteRepository (
  DontQuoteMe.Domain.QuoteRepository.FileBasedQuoteRepository.create
  ) where

import Control.Error.Util (hush)
import DontQuoteMe.Domain.QuoteRepository (Repository(..), QuoteRepository)
import DontQuoteMe.Domain.Quote as Quote
import qualified DontQuoteMe.Data.RDF as QRDF
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.Directory as D
import Data.Maybe (catMaybes)
import Data.RDF
import Data.UUID (UUID, fromString)
import System.FilePath ((</>), splitFileName, splitExtension)
import System.IO (withFile, IOMode(WriteMode), Handle)

quoteDir :: IO FilePath
quoteDir = do
  dir <- D.getXdgDirectory D.XdgData "dont-quote-me"
  D.createDirectoryIfMissing True dir
  pure dir

quoteToPath :: FilePath -> Quote -> FilePath
quoteToPath dir quote = dir </> (show $ getId quote) ++ ".ttl"

quoteForFile :: FilePath -> FilePath -> IO (Maybe Quote)
quoteForFile dir fileName = do
  let tp = TurtleParser Nothing Nothing
      fp = dir </> fileName
  eitherRDF <- parseFile tp fp
  let maybeQuote = do
        rdf <- hush eitherRDF
        uuid <- quoteIDFromFile fp
        QRDF.rdfToQuote uuid rdf
  pure maybeQuote

quoteIDFromFile :: FilePath -> Maybe UUID
quoteIDFromFile fp = fromString fileName
  where (_, fileNameWithExt) = splitFileName fp
        (fileName, _) = splitExtension fileNameWithExt

create :: IO QuoteRepository
create = do
  dir <- quoteDir
  let mappings = PrefixMappings $ M.fromList [("schema", "http://schema.org/")]
  pure $ Repo { getById = \_ -> return Nothing
              , getAll = do
                  fs <- D.listDirectory dir
                  quoteMaybes <- traverse (quoteForFile dir) fs
                  pure $ catMaybes quoteMaybes
              , save = \quote -> do
                  let graph = QRDF.quoteToRdfGraph quote
                      docUrl = T.concat ["localhost:3000/quote/", T.pack $ show $ getId quote]
                      serializer = TurtleSerializer (Just docUrl) mappings
                      quoteFilePath = quoteToPath dir quote
                  withFile quoteFilePath WriteMode (\handle -> hWriteRdf serializer handle graph)
              }
