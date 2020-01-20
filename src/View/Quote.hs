{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.Quote ( ViewQuote(..)
                  , ViewQuotee(..)
                  , toView) where

import Prelude hiding (fail)
import qualified Data.Text as T
import qualified Domain.Quote as Domain
import Control.Monad.IO.Class
import Control.Monad.Fail
import Network.URL (URL, exportURL)
import Data.RDF
import Data.Maybe (listToMaybe)
import Network.HTTP.Simple as HTTP
import qualified System.IO.Temp as Temp
import Data.ByteString as BS
import Control.Exception.Base

data ViewQuotee = Anonymous
                | Quotee T.Text

data ViewQuote = ViewQuote { quote :: T.Text
                           , quotee :: ViewQuotee
                           }

instance Show ViewQuotee where
  show Anonymous = "Anonymous"
  show (Quotee name) = T.unpack name

getTurtle :: (MonadIO m, MonadFail m) => URL -> m (RDF TList)
getTurtle url = do
  let eitherReq = do
        req <- parseRequest $ exportURL url
        pure $ setRequestHeader "Accept" ["text/turtle"] req
  req <- case eitherReq of
    Left (SomeException e) -> fail $ show e
    Right req -> pure req
  resp <- httpBS req
  tmpFilePath <- liftIO $ Temp.emptySystemTempFile "quote.rdf"
  liftIO $ BS.writeFile tmpFilePath (getResponseBody resp)
  eitherRDF <- liftIO $ parseFile (TurtleParser Nothing Nothing) tmpFilePath
  case eitherRDF of
    Left e -> fail $ (show e)
    Right rdf -> pure rdf

textFromNode :: (MonadFail m) => Node -> m T.Text
textFromNode (LNode (PlainLL (name :: T.Text) _)) = pure name
textFromNode _ = fail "Could not get person's name from object node of triple."

nameFromURL :: (MonadIO m, MonadFail m) => URL -> m T.Text
nameFromURL url = do
  graph <- getTurtle url
  let names = query graph Nothing (Just (unode "http://xmlns.com/foaf/0.1/name")) Nothing
  nameTriple <- case listToMaybe names of
    Just nn -> pure nn
    Nothing -> fail "Could not find this person's name."
  name <- textFromNode $ objectOf nameTriple
  return name

toView :: (MonadIO m, MonadFail m) => Domain.Quote -> m ViewQuote
toView (Domain.Quote _ quote Domain.Anonymous) = pure $ ViewQuote quote Anonymous
toView (Domain.Quote _ quote (Domain.Person url)) = do
  name <- nameFromURL url
  pure $ ViewQuote quote $ Quotee name
