{-# LANGUAGE OverloadedStrings #-}

module View.Html where

--import Text.Blaze ((!))
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5.Attributes
import qualified DontQuoteMe.View.Quote as V
import Data.Text as T
import Control.Monad (forM_)

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
quoteForm = H.form ! A.method "post" ! A.enctype "multipart/form-data" ! action "/quotes" $ do
  H.span $ H.toHtml ("Enter a quote:" :: Text)
  H.br
  H.input ! A.type_ "textarea" ! A.name "quote" ! A.id "quote-text"
  H.br
  H.span $ H.toHtml ("Who said it? (type at least three characters of a name or a URL that identifies a person):" :: Text)
  H.br
  H.input ! A.type_ "textarea" ! A.name "said_by" ! A.id "quotee-text"
  H.br
  H.input ! A.type_ "submit" ! A.value "Submit"

homeLink :: H.Html
homeLink = ((H.a . H.toHtml) ("Home" :: String)) ! A.href "/"

template :: String -> H.Html -> H.Html
template title' body' = H.docTypeHtml $ do
  H.head $ do
    H.title $ fromString $ "Don't Quote Me! - " ++ title'
    H.link ! A.rel "stylesheet" ! A.href "https://code.jquery.com/ui/1.12.1/themes/ui-lightness/jquery-ui.css"
    H.script ! A.src "https://code.jquery.com/jquery-3.4.1.min.js" $ ""
    H.script ! A.src "https://code.jquery.com/ui/1.12.0/jquery-ui.min.js" $ ""
    H.script ! A.src "js/dont-quote-me.js" $ ""
  H.body $ homeLink >> H.br >> body'

homeHtml :: [V.ViewQuote] -> H.Html
homeHtml quotes = template "Home" quoteForm >> (quotesHtml quotes)
