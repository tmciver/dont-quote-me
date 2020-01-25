module Controller.Home (
  Controller.Home.get
  ) where

import qualified DontQuoteMe.View.Quote as V
import DontQuoteMe.Domain.QuoteRepository
import qualified View.Html as HTML
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad.IO.Class
import Web.Scotty

get :: QuoteRepository -> ActionM ()
get repo = do
  quotes <- liftIO $ getAll repo
  viewQuotes <- liftIO $ traverse V.toView quotes
  html $ renderHtml $ HTML.homeHtml viewQuotes
