{-# LANGUAGE OverloadedStrings #-}

module Controller.Quote (
  Controller.Quote.post
  ) where

import Web.Scotty
import DontQuoteMe.Domain.Quote
import DontQuoteMe.Domain.QuoteRepository
import Data.Text as T
import Network.URL (importURL)
import Control.Monad.IO.Class

createQuote :: QuoteRepository
           -> Text
           -> Quotee
           -> ActionM ()
createQuote repo quoteText' quotee = do
  quote <- liftIO (create quoteText' quotee)
  liftIO (save repo quote)
  redirect "/"

parseQuotee :: Text
            -> Maybe Quotee
parseQuotee quoteeText | toLower quoteeText == "anonymous" = Just Anonymous
parseQuotee quoteeText = Person <$> importURL (T.unpack quoteeText)

post :: QuoteRepository -> ActionM ()
post repo = do
  quoteText' <- param "quote"
  quoteeText <- param "said_by"
  case parseQuotee quoteeText of
    Nothing -> raise "Invalid quotee"
    Just quotee -> createQuote repo quoteText' quotee
