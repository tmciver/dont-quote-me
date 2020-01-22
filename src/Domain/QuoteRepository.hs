module Domain.QuoteRepository ( Repository(..)
                              , QuoteRepository
                              ) where

import Domain.Quote
import Data.UUID (UUID)

data Repository i a = Repo { getById :: i -> IO (Maybe a)
                           , getAll :: IO [a]
                           , save :: a -> IO ()
                           }

type QuoteRepository = Repository UUID Quote
