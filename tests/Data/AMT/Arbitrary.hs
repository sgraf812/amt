{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.AMT.Arbitrary where

import           Data.AMT
import           Test.Tasty.QuickCheck

instance Arbitrary a => Arbitrary (AMT a) where
  arbitrary = fromList <$> arbitrary
  shrink = fmap fromList . shrink . toList
