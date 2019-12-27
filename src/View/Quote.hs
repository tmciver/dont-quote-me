{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.Quote where

import qualified Data.Text as T
import qualified Domain.Quote as Domain
import Control.Monad.IO.Class
import Control.Monad.Fail
import Network.URL (URL, exportURL)
import Data.RDF

data ViewQuotee = Anonymous
                | Quotee T.Text

data ViewQuote = ViewQuote { quote :: T.Text
                           , quotee :: ViewQuotee
                           }

instance Show ViewQuotee where
  show Anonymous = "Anonymous"
  show (Quotee name) = T.unpack name

nameFromURL :: (MonadIO m, MonadFail m) => URL -> m T.Text
nameFromURL url = do
  eitherRdf <- liftIO $ parseURL (TurtleParser Nothing Nothing) (exportURL url)
  let Right (rdf :: RDF TList) = eitherRdf
      --names = query rdf (Just (UNode (T.pack (exportURL url)))) (Just (UNode "http://xmlns.com/foaf/0.1/name")) Nothing
      --names = query rdf (Just (UNode (T.pack (exportURL url)))) (Just (UNode "foaf:name")) Nothing
      names = query rdf Nothing (Just (unode "http://xmlns.com/foaf/0.1/name")) Nothing
      (LNode (PlainLL (name :: T.Text) _)) = objectOf $ head names
  return name

toView :: (MonadIO m, MonadFail m) => Domain.Quote -> m ViewQuote
toView (Domain.Quote _ quote Domain.Anonymous) = pure $ ViewQuote quote Anonymous
toView (Domain.Quote _ quote (Domain.Person url)) = do
  name <- nameFromURL url
  pure $ ViewQuote quote $ Quotee name

