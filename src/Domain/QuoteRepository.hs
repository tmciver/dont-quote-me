{-# LANGUAGE OverloadedStrings #-}

module Domain.QuoteRepository ( QuoteRepository
                             , inMemoryQuoteRepo
                             , getAll
                             , save
                             ) where

import Domain.Quote as Quote
import Control.Concurrent.STM
import Data.UUID
import Network.URL (importURL)
import Data.Maybe (fromJust)

type ID = UUID

data Repository i a = Repo { getById :: i -> IO (Maybe a)
                           , getAll :: IO [a]
                           , save :: a -> IO ()
                           }

type QuoteRepository = Repository ID Quote

inMemoryQuoteRepo :: IO QuoteRepository
inMemoryQuoteRepo = do
  quotesTVar <- newTVarIO []
  let repo = Repo { getById = \_ -> return Nothing
                  , getAll = readTVarIO quotesTVar
                  , save = \quote -> atomically $ modifyTVar' quotesTVar (quote:)
                  }
      lennon = Person $ fromJust $ importURL "http://dbpedia.org/resource/John_Lennon"
  (Quote.create "Life is what happens when you're busy making other plans." lennon) >>= save repo
  (Quote.create "It is difficult to soar with eagles when you work with turkeys." Anonymous) >>= save repo
  return repo
