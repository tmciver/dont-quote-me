{-# LANGUAGE OverloadedStrings #-}

module DontQuoteMe.Domain.Quote where

import Data.Text
import Network.URL (URL)
import Data.UUID
import Data.UUID.V4 (nextRandom)
import qualified Data.Text as T
import Network.URL (URL, importURL, exportURL)
import Data.Maybe (fromJust)

data Quotee = Anonymous
            | Person URL
  deriving (Eq, Show)

data Quote = Quote { getId :: UUID
                   , quoteText :: Text
                   , saidBy :: Quotee
                   } deriving (Eq, Show)

create :: Text
       -> Quotee
       -> IO Quote
create quote quotee = do
  uuid <- nextRandom
  pure $ Quote { getId = uuid, quoteText = quote, saidBy = quotee }

anonymousURI :: URL
anonymousURI = fromJust $ importURL "http://www.wikidata.org/wiki/Q4233718"

quoteeURI :: Quotee -> URL
quoteeURI Anonymous = anonymousURI
quoteeURI (Person url) = url

quoteeURIText :: Quotee -> T.Text
quoteeURIText quotee = T.pack $ exportURL $ quoteeURI quotee

