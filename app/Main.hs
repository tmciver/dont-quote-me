{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import DontQuoteMe.Domain.QuoteRepository
--import qualified Domain.QuoteRepository.InMemoryQuoteRepository as InMemoryQuoteRepository
--import qualified Domain.QuoteRepository.FileBasedQuoteRepository as FileBasedQuoteRepository
import qualified DontQuoteMe.Domain.QuoteRepository.LinkedDataQuoteRepository as LinkedDataQuoteRepository
import qualified Controller.Quote as QuoteController
import qualified Controller.Home as HomeController

main :: IO ()
main = do
  --repo <- InMemoryQuoteRepository.create
  --repo <-FileBasedQuoteRepository.create
  let repo = LinkedDataQuoteRepository.create
  scotty 3000 $ do
    get "/" (HomeController.get repo)
    post "/quotes" (QuoteController.post repo)
