{-# LANGUAGE OverloadedStrings #-}

module DontQuoteMe.Data.RDF where

import Domain.Quote
import Data.UUID (UUID)
import Data.RDF
import qualified Data.Text as T
import Network.URL (importURL)
import Data.Maybe (listToMaybe)
import Data.Functor ((<&>))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Exception.Base
import Data.Typeable
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.URL (URL, exportURL)
import qualified System.IO.Temp as Temp
import Data.ByteString as BS hiding (empty)
import Network.HTTP.Simple (parseRequest, httpBS, setRequestHeader, getResponseBody)

mkPrefixedNode :: T.Text -> T.Text -> Node
mkPrefixedNode prefix term = (unode (T.concat [prefix, term]))

rdfPrefix :: T.Text
rdfPrefix = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

schemaPrefix :: T.Text
schemaPrefix = "http://schema.org/"

ldpPrefix :: T.Text
ldpPrefix = "http://www.w3.org/ns/ldp#"

nodesToQuote :: UUID -> Node -> Node -> Maybe Quote
nodesToQuote uuid (UNode uriText) (LNode (PlainL quoteText)) = do
  personURI <- importURL (T.unpack uriText)
  pure $ Quote uuid quoteText (Person personURI)
nodesToQuote _ quoteeURINode quoteTextNode = Nothing

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

rdfToQuote :: UUID -> RDF TList -> Maybe Quote
rdfToQuote uuid rdf = do
  _ <- listToMaybe $ query rdf Nothing (Just (mkPrefixedNode rdfPrefix "type")) (Just (mkPrefixedNode schemaPrefix "Quotation"))
  quoteeTriple <- listToMaybe $ query rdf Nothing (Just (mkPrefixedNode schemaPrefix "spokenByCharacter")) Nothing
  quoteTextTriple <- listToMaybe $ query rdf Nothing (Just (mkPrefixedNode schemaPrefix "text")) Nothing
  let quotee = objectOf quoteeTriple
      quoteText = objectOf quoteTextTriple
  nodesToQuote uuid quotee quoteText

data ParseException = RDF4HParseException ParseFailure
  deriving Typeable

instance Show ParseException where
  show (RDF4HParseException (ParseFailure s)) = s

instance Exception ParseException

getTurtle :: (MonadIO m, MonadThrow m) => URL -> m (RDF TList)
getTurtle url = do
  req <- (parseRequest $ exportURL url) <&> setRequestHeader "Accept" ["text/turtle"]
  resp <- liftIO $ httpBS req
  tmpFilePath <- liftIO $ Temp.emptySystemTempFile "quote.rdf"
  liftIO $ BS.writeFile tmpFilePath (getResponseBody resp)
  eitherRDF <- liftIO $ parseFile (TurtleParser Nothing Nothing) tmpFilePath
  case eitherRDF of
    Left e -> throwM $ RDF4HParseException e
    Right rdf -> pure rdf

