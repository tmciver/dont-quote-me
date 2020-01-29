{-# LANGUAGE DeriveGeneric #-}

module DontQuoteMe.Domain.PersonQuery where

import GHC.Generics
import Data.Text

data Person = Person { uri :: Text
                     , name :: Text
                     }
              deriving (Show, Eq, Generic)

type PersonQuery = String -> IO [Person]
