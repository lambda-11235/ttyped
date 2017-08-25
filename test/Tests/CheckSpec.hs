
module Tests.CheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Check
import Reduce
import Representation
import Tests.ReprGen
import Tests.Util


spec :: Spec
spec = do
  describe "checkTerm" $ do
    it "reduction of type checked term produces normal form" $ property $ \t ->
      typeChecks t ==> (isInNormalForm (reduceTerm t))

    -- I hate unit tests, but I should at least test this term.
    it "\\a : *. \\x : a. x has type @a : *. @x : a. a" $
      checkObject (Fun (Just "a") (C Star) (Fun (Just "x") (O (Var "a" 0)) (Var "x" 0))) Star
      `shouldBe`
      Right (O (Prod (Just "a") (C Star) (Prod (Just "x") (O (Var "a" 0)) (Var "a" 1))))
