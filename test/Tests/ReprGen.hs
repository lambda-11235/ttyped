module Tests.ReprGen where

import Test.QuickCheck

import Representation


instance Arbitrary Term where
  arbitrary = sized termGen

  shrink (C ctx) = map C (shrink ctx)
  shrink (O obj) = map O (shrink obj)

instance Arbitrary Context where
  arbitrary = sized contextGen

  shrink Star = []
  shrink (Quant arg term ctx) = (Quant arg) <$> (shrink term) <*> (shrink ctx)

instance Arbitrary Object where
  arbitrary = sized objectGen

  shrink (Var _ _) = []
  shrink (Prod arg term obj) = (Prod arg) <$> (shrink term) <*> (shrink obj)
  shrink (Fun arg term obj) = (Fun arg) <$> (shrink term) <*> (shrink obj)
  shrink (App obj1 obj2) = [obj1, obj2]


termGen :: Int -> Gen Term
termGen n = oneof [C <$> contextGen n, O <$> objectGen n]

contextGen :: Int -> Gen Context
contextGen 0 = return Star
contextGen n = do argName <- arbitrary
                  term <- termGen (n `div` 2)
                  ctx <- contextGen (n `div` 2)
                  elements [Star, Quant argName term ctx]

objectGen :: Int -> Gen Object
objectGen 0 = Var <$> arbitrary <*> arbitrary
objectGen n = do argName <- arbitrary
                 argNameM <- arbitrary
                 idx <- arbitrary
                 term <- termGen (n `div` 2)
                 obj1 <- objectGen (n `div` 2)
                 obj2 <- objectGen (n `div` 2)
                 elements [ Var argName idx
                          , Prod argNameM term obj1
                          , Fun argNameM term obj1
                          , App obj1 obj2 ]
