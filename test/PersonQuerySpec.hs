module PersonQuerySpec where

import Test.Hspec
import qualified DontQuoteMe.Domain.PersonQuery.QueryLengthConstraint as QLC
import qualified DontQuoteMe.Domain.PersonQuery.InMemoryPersonQuery as PQ
import Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = do
  describe "QueryLengthConstraint" $ do
    let minChars = 3
        pq = QLC.create PQ.create minChars
    it "should work normally when the query is at least the minimum length" $ do
      r <- liftIO $ pq "abc"
      length r `shouldBe` 1
    it "should throw an exception when the query is too short" $ do
      pq "ab" `shouldThrow` (== QLC.TooFewQueryChars minChars)
