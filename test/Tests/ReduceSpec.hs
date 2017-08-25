
module Tests.ReduceSpec (spec) where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Check
import Reduce
import Representation
import Tests.ReprGen
import Tests.Util


spec :: Spec
spec = do
  describe "reduceTerm" $
    modifyMaxDiscardRatio (10 *) $ do
      it "reducing twice produces the same result" $ property $ \obj ctx ->
        isRight (checkObject obj ctx) ==>
          (let obj' = reduceObject obj in obj' == reduceObject obj')

      it "reduction preserves types" $ property $ \obj ctx ->
        let typ = checkObject obj ctx in
          isRight typ ==> (typ == checkObject (reduceObject obj) ctx)
