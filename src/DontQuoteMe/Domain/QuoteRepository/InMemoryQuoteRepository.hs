{-# LANGUAGE OverloadedStrings #-}

module DontQuoteMe.Domain.QuoteRepository.InMemoryQuoteRepository (
  DontQuoteMe.Domain.QuoteRepository.InMemoryQuoteRepository.create
  ) where

import DontQuoteMe.Domain.QuoteRepository (Repository(..), QuoteRepository)
import DontQuoteMe.Domain.Quote as Quote
import Control.Concurrent.STM
import Data.Maybe (fromJust)
import Network.URL (importURL)

create :: IO QuoteRepository
create = do
  quotesTVar <- newTVarIO []
  let repo = Repo { getById = \_ -> return Nothing
                  , getAll = readTVarIO quotesTVar
                  , save = \quote -> atomically $ modifyTVar' quotesTVar (quote:)
                  }
      lennon = Person $ fromJust $ importURL "http://dbpedia.org/resource/John_Lennon"
  (Quote.create "Life is what happens when you're busy making other plans." lennon) >>= save repo
  (Quote.create "It is difficult to soar with eagles when you work with turkeys." Anonymous) >>= save repo
  return repo
