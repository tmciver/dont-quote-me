{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty
import DontQuoteMe.Domain.QuoteRepository
import qualified DontQuoteMe.Domain.QuoteRepository.InMemoryQuoteRepository as InMemoryQuoteRepository
import qualified DontQuoteMe.Domain.QuoteRepository.FileBasedQuoteRepository as FileBasedQuoteRepository
import qualified DontQuoteMe.Domain.QuoteRepository.LinkedDataQuoteRepository as LinkedDataQuoteRepository
import qualified Controller.Quote as QuoteController
import qualified Controller.Home as HomeController
import Dhall
import Control.Exception (catch, SomeException)

data RepoPath = Default
              | RepoPath FilePath
  deriving (Generic, Show)

data RepoConfig = InMemory
                | FileBased RepoPath
                | LinkedData String
  deriving (Generic, Show)

data Config = Config { repository :: RepoConfig }
  deriving (Generic, Show)

instance Interpret RepoPath
instance Interpret RepoConfig
instance Interpret Config

defaultConfig :: Config
defaultConfig = Config InMemory

toRepository :: RepoConfig -> IO QuoteRepository
toRepository InMemory = InMemoryQuoteRepository.create
toRepository (FileBased Default) = FileBasedQuoteRepository.create
toRepository (FileBased _) = error "Not yet implemented"
toRepository (LinkedData ignoredForNow) = pure LinkedDataQuoteRepository.create

getConfig :: IO Config
getConfig = (input auto "./config.dhall") `catch`
  (\(e :: SomeException) -> do
      print e
      putStrLn "Could not load Dhall configuration file; using default configuration."
      pure defaultConfig)

main :: IO ()
main = do
  config <- getConfig
  print config
  repo <- toRepository (repository config)
  scotty 3000 $ do
    get "/" (HomeController.get repo)
    post "/quotes" (QuoteController.post repo)
