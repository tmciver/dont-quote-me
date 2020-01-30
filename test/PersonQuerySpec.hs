module PersonQuerySpec where

import Test.Hspec
import qualified DontQuoteMe.Domain.PersonQuery.QueryLengthConstraint as QLC
import qualified DontQuoteMe.Domain.PersonQuery.InMemoryPersonQuery as PQ
import Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = do
  describe "InMemoryPersonQuery" $ do
    let pq = PQ.create
    it "should return no results for a query that does not match any `Person`" $ do
      r <- liftIO $ pq "ab"
      r `shouldBe` []
    it "should return multiple matching results" $ do
      r <- liftIO $ pq "mal"
      length r `shouldBe` 2
  describe "QueryLengthConstraint" $ do
    let minChars = 3
        pq = QLC.create PQ.create minChars
    it "should work normally when the query is at least the minimum length" $ do
      r <- liftIO $ pq "mal"
      length r `shouldBe` 2
    it "should throw an exception when the query is too short" $ do
      pq "ma" `shouldThrow` (== QLC.TooFewQueryChars minChars)
