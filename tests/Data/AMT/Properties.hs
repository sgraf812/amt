{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.AMT.Properties where


import           Data.AMT
import           Data.AMT.Arbitrary    ()
import qualified Data.List             as List
import           Data.Word
import           Prelude               hiding (filter, lookup, map, max, null)
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

emptyAssocs :: [(Word64, Word64)]
emptyAssocs = []

spec :: Spec
spec =
  describe "AMT" $ do
    describe "empty" $
      it "fromList []" $ fromList emptyAssocs `shouldBe` empty

      -- it "is null" $ null empty `shouldBe` True
      -- it "has size 0" $ size empty `shouldBe` 0
{-     describe "singleton" $ do
      let m = singleton 1 1
      it "fromList [(k, v)]" $ fromList (divisibility 1) `shouldBe` m
      it "is not null" $ null m `shouldBe` False
      it "has size 1" $ size m `shouldBe` 1 -}

{-     describe "member" $
      it "coincides with List.lookup" $
        property $ \k (assocs :: [(Word64, Word64)]) ->
          member k (fromList assocs) `shouldBe` List.member k assocs -}

    describe "lookup" $
      it "coincides with List.lookup" $
        property $ \k (assocs :: [(Word64, Word64)]) ->
          lookup k (fromList assocs) `shouldBe` List.lookup k assocs

    describe "insert" $
      it "overwrites an entry" $
        property $ \(m :: AMT Int) k v ->
          lookup k (insert k v m) `shouldBe` Just v
