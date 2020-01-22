{-# LANGUAGE OverloadedStrings #-}

module View.Html where

import qualified Text.Blaze.Html5 as H
import qualified View.Quote as V
import Data.Text as T
import Control.Monad (forM_)
import Text.Blaze.Html5.Attributes
import Data.String (fromString)

quoteToListItemText :: V.ViewQuote -> T.Text
quoteToListItemText (V.ViewQuote quote quotee) = T.concat ["\"", quote, "\" - ", T.pack $ show quotee]

quoteHtml :: V.ViewQuote -> H.Html
quoteHtml = H.li . fromString . T.unpack . quoteToListItemText

quotesHtml :: [V.ViewQuote] -> H.Html
quotesHtml quotes = do
  H.p "Quotes:"
  H.ul $ forM_ quotes quoteHtml

quoteForm :: H.Html
quoteForm = H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/quotes" $ do
  H.span $ H.toHtml ("Enter a quote:" :: Text)
  H.br
  H.input H.! type_ "textarea" H.! name "quote"
  H.br
  H.span $ H.toHtml ("Who said it? (Enter \"Anonymous\" or a URL that identifies a person):" :: Text)
  H.br
  H.input H.! type_ "textarea" H.! name "said_by"
  H.br
  H.input H.! type_ "submit" H.! value "Submit"

homeLink :: H.Html
homeLink = ((H.a . H.toHtml) ("Home" :: String)) H.! href "/"

template :: String -> H.Html -> H.Html
template title' body' = H.docTypeHtml $ do
  H.head $ do
    H.title $ fromString $ "Don't Quote Me! - " ++ title'
  H.body $ homeLink >> H.br >> body'

homeHtml :: [V.ViewQuote] -> H.Html
homeHtml quotes = template "Home" quoteForm >> (quotesHtml quotes)
