{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Static
--import qualified Domain.QuoteRepository.InMemoryQuoteRepository as InMemoryQuoteRepository
--import qualified Domain.QuoteRepository.FileBasedQuoteRepository as FileBasedQuoteRepository
import qualified DontQuoteMe.Domain.QuoteRepository.LinkedDataQuoteRepository as LinkedDataQuoteRepository
--import qualified DontQuoteMe.Domain.PersonQuery.InMemoryPersonQuery as InMemoryPersonQuery
import qualified DontQuoteMe.Domain.PersonQuery.SparqlPersonQuery as SparqlPersonQuery
import qualified Controller.Quote as QuoteController
import qualified Controller.People as PeopleController
import qualified Controller.Home as HomeController

main :: IO ()
main = do
  --repo <- InMemoryQuoteRepository.create
  --repo <-FileBasedQuoteRepository.create
  let repo = LinkedDataQuoteRepository.create
  --let personQuery = InMemoryPersonQuery.create
  let personQuery = SparqlPersonQuery.create
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/" (HomeController.get repo)
    get "/people" (PeopleController.get personQuery)
    post "/quotes" (QuoteController.post repo)
