
module ReduceSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import ReprGen
import Reduce

spec :: Spec
spec = do
  describe "reduceTerm" $ do
    it "reducing twice produces the same result" $ property $ \t ->
      let t' = reduceTerm t in
          t' == reduceTerm t'
