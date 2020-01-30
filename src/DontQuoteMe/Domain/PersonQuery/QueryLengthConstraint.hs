module DontQuoteMe.Domain.PersonQuery.QueryLengthConstraint where

import DontQuoteMe.Domain.PersonQuery
import Control.Exception.Base
import Data.Typeable

data QueryException = TooFewQueryChars Int
  deriving (Eq, Typeable)

instance Show QueryException where
  show (TooFewQueryChars minChars) = "Not enough characters supplied for the query; expect at least" <> show minChars

instance Exception QueryException

-- |Creates a new 'PersonQuery' that ensures that the query text contains at
-- least the number of characters given by the second argument.
create :: PersonQuery
       -- ^The query being wrapped
       -> Int
       -- ^The minimum number of characters accepted by the returned
       -- 'PersonQuery'
       -> PersonQuery
create pq numChars = \q -> if length q < numChars
                           then throwIO $ TooFewQueryChars numChars
                           else pq q
