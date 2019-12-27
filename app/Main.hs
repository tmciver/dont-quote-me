{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Domain.Quote
import Domain.QuoteRepository
import qualified View.Quote as V
import Data.Text as T
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.String (fromString)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Network.URL (importURL)

quoteToListItemText :: V.ViewQuote -> T.Text
quoteToListItemText (V.ViewQuote quote quotee) = T.concat ["\"", quote, "\" - ", T.pack $ show quotee]

quoteHtml :: V.ViewQuote -> H.Html
quoteHtml = H.li . fromString . T.unpack . quoteToListItemText

quotesHtml :: [V.ViewQuote] -> H.Html
quotesHtml quotes = do
  H.p "Quotes:"
  --H.ul $ forM_ quotes (H.li . fromString . T.unpack . desc)
  H.ul $ forM_ quotes quoteHtml

quoteForm :: H.Html
quoteForm = H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/quotes" $ do
  H.span $ H.toHtml ("Enter a quote:" :: Text)
  H.br
  H.input H.! type_ "textarea" H.! name "quote"
  H.br
  H.span $ H.toHtml ("Who said it? (Enter \"Anonymous\" or a URL that returns .ttl):" :: Text)
  H.br
  H.input H.! type_ "textarea" H.! name "said_by"
  H.br
  H.input H.! type_ "submit" H.! value "Submit"

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

homeLink :: H.Html
homeLink = ((H.a . H.toHtml) ("Home" :: String)) H.! href "/"

template :: String -> H.Html -> H.Html
template title' body' = H.docTypeHtml $ do
  H.head $ do
    H.title $ fromString $ "Don't Quote Me! - " ++ title'
  H.body $ homeLink >> H.br >> body'

homeView :: QuoteRepository -> ActionM ()
homeView repo = do
  quotes <- liftIO $ getAll repo
  viewQuotes <- liftIO $ traverse V.toView quotes
  html $ renderHtml $ homeHtml viewQuotes

homeHtml :: [V.ViewQuote] -> H.Html
homeHtml quotes = template "Home" quoteForm >> (quotesHtml quotes)

main :: IO ()
main = do
  repo <- inMemoryQuoteRepo
  scotty 3000 $ do
    get "/" (homeView repo)
    post "/quotes" (handleQuotePost repo)
