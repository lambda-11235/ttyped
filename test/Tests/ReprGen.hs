module Tests.ReprGen where

import Test.QuickCheck

import Representation


-- NOTE: When generating values we assume variable names won't be used in tests,
-- and therefore don't assign meaningful names in expressions.


instance Arbitrary Term where
  arbitrary = sized (termGen 0)

  shrink (C ctx) = map C (shrink ctx)
  shrink (O obj) = map O (shrink obj)

instance Arbitrary Context where
  arbitrary = sized (contextGen 0)

  shrink Star = []
  shrink (Quant arg term ctx) = ((Quant arg) <$> (shrink term) <*> (pure ctx))
    ++ ((Quant arg) <$> (pure term) <*> (shrink ctx))

instance Arbitrary Object where
  arbitrary = sized (objectGen 0)

  shrink (Var _ _) = []
  shrink (Prod arg term obj) = ((Prod arg) <$> (shrink term) <*> (pure obj))
    ++ ((Prod arg) <$> (pure term) <*> (shrink obj))
  shrink (Fun arg term obj) = ((Fun arg) <$> (shrink term) <*> (pure obj))
    ++ ((Fun arg) <$> (pure term) <*> (shrink obj))
  shrink (App obj1 obj2) = [obj1, obj2]


termGen :: Nat -> Int -> Gen Term
termGen depth n = oneof [C <$> contextGen depth n, O <$> objectGen depth n]

contextGen :: Nat -> Int -> Gen Context
contextGen depth 0 = return Star
contextGen depth n =
  oneof [ return Star
        , Quant Nothing <$> (termGen depth (n `div` 2)) <*> (contextGen (depth + 1) (n - 1)) ]

objectGen :: Nat -> Int -> Gen Object
objectGen depth 0 = if depth <= 0
                    then return (Fun Nothing (C Star) (Var defVarName 0))
                    else Var defVarName <$> (choose (0, depth - 1))
objectGen depth n = oneof [ Var defVarName <$> (choose (0, depth - 1))
                          , Prod Nothing <$> (termGen depth (n `div` 2)) <*> (objectGen (depth + 1) (n - 1))
                          , Fun Nothing <$> (termGen depth (n `div` 2)) <*> (objectGen (depth + 1) (n - 1))
                          , App <$> (objectGen depth (n `div` 2)) <*> (objectGen depth (n `div` 2)) ]


-- | Default variable name used when we want to ignore a variable.
defVarName :: String
defVarName = "x"
