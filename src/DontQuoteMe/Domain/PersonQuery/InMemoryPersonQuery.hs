{-# LANGUAGE OverloadedStrings #-}

module DontQuoteMe.Domain.PersonQuery.InMemoryPersonQuery where

import DontQuoteMe.Domain.PersonQuery
import qualified Data.Text as T

create :: PersonQuery
create query = pure $ filter nameMatches personDb
  where qText = T.toLower $ T.pack query
        nameMatches :: Person -> Bool
        nameMatches (Person {name=name}) = qText `T.isInfixOf` (T.toLower name)
        personDb = [ Person "http://dbpedia.org/resource/Malcolm_Reynolds" "Malcolm Reynolds"
                   , Person "http://dbpedia.org/resource/Malcolm_O'Kelly" "Malcolm O'Kelly"
                   , Person "http://dbpedia.org/resource/Jeffrey_Lebowski" "The Dude"]
