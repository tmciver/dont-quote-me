{-# LANGUAGE OverloadedStrings #-}

module DontQuoteMe.Domain.PersonQuery.SparqlPersonQuery (create) where

import DontQuoteMe.Domain.PersonQuery
import qualified Data.Text as T
import Data.RDF (Node(LNode, UNode), LValue(PlainLL))
import Database.HSparql.QueryGenerator
import Database.HSparql.Connection
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Maybe (fromMaybe)
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

-- |Converts each space character in the string to the two-character sequence
-- "\ ". This is done so that a SPARQL query containing a space will succeed.
escapeSpace :: String -> String
escapeSpace s = s >>= f
  where f ' ' = "\\ "
        f c = [c]

sanitize :: String -> String
sanitize = escapeSpace

personQueryDbpedia :: PersonQuery
personQueryDbpedia q = do
  let q' = sanitize q
      query = (personSparqlQuery $ T.pack q')
  maybeResults <- selectQuery "http://dbpedia.org/sparql" query
  let maybePeople = maybeResults >>= traverse bindingsToPerson
  pure $ fromMaybe [] maybePeople

create :: PersonQuery
create = personQueryDbpedia
