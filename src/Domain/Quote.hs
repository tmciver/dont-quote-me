{-# LANGUAGE OverloadedStrings #-}

module Domain.Quote where

import Data.Text
import Network.URL (URL)
import Data.UUID
import Data.UUID.V4 (nextRandom)

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
