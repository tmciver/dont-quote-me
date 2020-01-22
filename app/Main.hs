{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Domain.Quote
import Domain.QuoteRepository
--import qualified Domain.QuoteRepository.InMemoryQuoteRepository as InMemoryQuoteRepository
--import qualified Domain.QuoteRepository.FileBasedQuoteRepository as FileBasedQuoteRepository
import qualified Domain.QuoteRepository.LinkedDataQuoteRepository as LinkedDataQuoteRepository
import qualified View.Quote as V
import qualified View.Html as HTML
import Data.Text as T
import Control.Monad.IO.Class
import Network.URL (importURL)
import Text.Blaze.Html.Renderer.Text (renderHtml)

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

handleQuotePost :: QuoteRepository -> ActionM ()
handleQuotePost repo = do
  quoteText' <- param "quote"
  quoteeText <- param "said_by"
  case parseQuotee quoteeText of
    Nothing -> raise "Invalid quotee"
    Just quotee -> createQuote repo quoteText' quotee

homeView :: QuoteRepository -> ActionM ()
homeView repo = do
  quotes <- liftIO $ getAll repo
  viewQuotes <- liftIO $ traverse V.toView quotes
  html $ renderHtml $ HTML.homeHtml viewQuotes

main :: IO ()
main = do
  --repo <- InMemoryQuoteRepository.create
  --repo <-FileBasedQuoteRepository.create
  let repo = LinkedDataQuoteRepository.create
  scotty 3000 $ do
    get "/" (homeView repo)
    post "/quotes" (handleQuotePost repo)
