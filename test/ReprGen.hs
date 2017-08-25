module ReprGen where

import Test.QuickCheck

import Representation


instance Arbitrary Term where
  arbitrary = sized termGen

instance Arbitrary Context where
  arbitrary = sized contextGen

instance Arbitrary Object where
  arbitrary = sized objectGen


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
