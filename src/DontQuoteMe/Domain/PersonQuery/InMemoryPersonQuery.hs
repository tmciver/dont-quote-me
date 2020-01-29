{-# LANGUAGE OverloadedStrings #-}

module DontQuoteMe.Domain.PersonQuery.InMemoryPersonQuery where

import DontQuoteMe.Domain.PersonQuery

create :: PersonQuery
create query = pure [Person "http://dbpedia.org/resource/Malcolm_Reynolds" "Malcolm Reynolds"]
