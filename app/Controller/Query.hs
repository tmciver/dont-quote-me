{-# LANGUAGE OverloadedStrings #-}

module Controller.Query where

import Web.Scotty
import DontQuoteMe.Domain.PersonQuery
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)

instance ToJSON Person
instance FromJSON Person

get :: PersonQuery -> ActionM ()
get pq = do
  q <- param "q"
  results <- liftIO (pq q)
  json results
