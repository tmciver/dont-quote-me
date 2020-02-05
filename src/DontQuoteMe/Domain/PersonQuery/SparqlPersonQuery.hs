{-# LANGUAGE OverloadedStrings #-}

module DontQuoteMe.Domain.PersonQuery.SparqlPersonQuery (create) where

import DontQuoteMe.Domain.PersonQuery
import qualified Data.Text as T
import Data.RDF (Node(LNode, UNode), LValue(PlainLL))
import Database.HSparql.QueryGenerator
import Database.HSparql.Connection
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Maybe (maybe)
import Data.Typeable
import Control.Exception.Base

personSparqlQuery :: T.Text -> Query SelectQuery
personSparqlQuery q = do
  foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
  dbo <- prefix "dbo" (iriRef "http://dbpedia.org/ontology/")
  rdf <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

  name <- var
  uri <- var

  triple_ uri (rdf .:. "type") (dbo .:. "Person")
  triple_ uri (foaf .:. "name") name

  filterExpr_ $ regex name q
  limit_ 100

  selectVars [name, uri]

data SparqlException = InvalidQueryResultBindings
  deriving (Show, Typeable)
instance Exception SparqlException

bindingsToPerson :: MonadThrow m => [BindingValue] -> m Person
bindingsToPerson [(Bound (LNode (PlainLL name' _))), (Bound (UNode uri'))] =
  pure $ Person uri' name'
bindingsToPerson _ = throwM InvalidQueryResultBindings

personQueryDbpedia :: PersonQuery
personQueryDbpedia q = do
  maybeResults <- selectQuery "http://dbpedia.org/sparql" (personSparqlQuery $ T.pack q)
  let maybePeople = maybeResults >>= traverse bindingsToPerson
  pure $ maybe [] id maybePeople

create :: PersonQuery
create = personQueryDbpedia
