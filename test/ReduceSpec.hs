
module ReduceSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import ReprGen
import Reduce
import Util


spec :: Spec
spec = do
  describe "reduceTerm" $ do
    it "reducing twice produces the same result" $ property $ \t ->
      typeChecks t ==> (let t' = reduceTerm t in t' == reduceTerm t')
