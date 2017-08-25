
module CheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Check
import ReprGen
import Reduce
import Util


spec :: Spec
spec = do
  describe "checkTerm" $ do
    it "reduction of type checked term produces normal form" $ property $ \t ->
      typeChecks t ==> (isInNormalForm (reduceTerm t))
