{-# LANGUAGE OverloadedStrings #-}

module View.Quote where

import qualified Data.Text as T
import qualified Domain.Quote as Domain
import Control.Monad.IO.Class
import Network.URL (URL)

data ViewQuotee = Anonymous
                | Quotee T.Text

data ViewQuote = ViewQuote { quote :: T.Text
                           , quotee :: ViewQuotee
                           }

instance Show ViewQuotee where
  show Anonymous = "Anonymous"
  show (Quotee name) = T.unpack name

nameFromURL :: MonadIO m => URL -> m T.Text
nameFromURL _ = pure "Somebody Important"

toView :: MonadIO m => Domain.Quote -> m ViewQuote
toView (Domain.Quote _ quote Domain.Anonymous) = pure $ ViewQuote quote Anonymous
toView (Domain.Quote _ quote (Domain.Person url)) = do
  name <- nameFromURL url
  pure $ ViewQuote quote $ Quotee name

