{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DontQuoteMe.View.Quote (
  ViewQuote(..),
  ViewQuotee(..),
  toView) where

import qualified Data.Text as T
import qualified DontQuoteMe.Domain.Quote as DontQuoteMe.Domain
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)
import Network.URL (URL)
import qualified DontQuoteMe.Data.RDF as QRDF
import Data.Maybe (listToMaybe)
import Data.RDF

data ViewQuotee = Anonymous
                | Quotee T.Text

data ViewQuote = ViewQuote { quote :: T.Text
                           , quotee :: ViewQuotee
                           }

instance Show ViewQuotee where
  show Anonymous = "Anonymous"
  show (Quotee name) = T.unpack name

textFromNode :: (MonadThrow m) => Node -> m T.Text
textFromNode (LNode (PlainLL (name :: T.Text) _)) = pure name
textFromNode _ = fail "Could not get person's name from object node of triple."

nameFromURL :: (MonadIO m, MonadThrow m) => URL -> m T.Text
nameFromURL url = do
  graph <- QRDF.getTurtle url
  let names = query graph Nothing (Just (unode "http://xmlns.com/foaf/0.1/name")) Nothing
  nameTriple <- case listToMaybe names of
    Just nn -> pure nn
    Nothing -> fail "Could not find this person's name."
  name <- textFromNode $ objectOf nameTriple
  return name

toView :: (MonadIO m, MonadThrow m) => DontQuoteMe.Domain.Quote -> m ViewQuote
toView (DontQuoteMe.Domain.Quote _ quote DontQuoteMe.Domain.Anonymous) = pure $ ViewQuote quote Anonymous
toView (DontQuoteMe.Domain.Quote _ quote (DontQuoteMe.Domain.Person url)) = do
  name <- nameFromURL url
  pure $ ViewQuote quote $ Quotee name
