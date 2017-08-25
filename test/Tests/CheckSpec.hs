
module Tests.CheckSpec (spec) where

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
  describe "checkTerm" $
    modifyMaxDiscardRatio (10 *) $ do
      it "reduction of type checked term produces normal form" $ property $ \obj ->
        isRight (checkObject obj Star) ==> (nfObject (reduceObject obj))

      -- I hate unit tests, but I should at least test this term.
      it "\\a : *. \\x : a. x has type @a : *. @x : a. a" $
        checkObject (Fun (Just "a") (C Star) (Fun (Just "x") (O (Var "a" 0)) (Var "x" 0))) Star
        `shouldBe`
        Right (O (Prod (Just "a") (C Star) (Prod (Just "x") (O (Var "a" 0)) (Var "a" 1))))
