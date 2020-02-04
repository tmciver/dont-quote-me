{-# LANGUAGE OverloadedStrings #-}

module DontQuoteMe.Domain.PersonQuery.SparqlPersonQuery where

import DontQuoteMe.Domain.PersonQuery
import qualified Data.Text as T
import Database.HSparql.QueryGenerator

personSparqlQuery :: T.Text -> Query SelectQuery
personSparqlQuery q = do
  foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
  dbo <- prefix "dbo" (iriRef "http://dbpedia.org/ontology/")
  rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

  -- x <- var
  name <- var
  uri <- var

  triple_ uri (rdf .:. "type") (dbo .:. "Person")
  triple_ uri (foaf .:. "name") name

  filterExpr_ $ regex name q
  limit_ 100

  selectVars [name, uri]

create :: PersonQuery
create  = undefined
