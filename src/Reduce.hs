
module Reduce (reduce, subst) where

import Representation

-- | Reduces a type checked expression to normal form.
reduce :: Expr -> Expr
reduce (Universe level) = Universe level
reduce (Pi ta tb) = Pi (reduce ta) (reduce tb)
reduce (Lambda t b) = Lambda (reduce t) (reduce b)
reduce (Apply e1 e2) = apply (reduce e1) (reduce e2)
reduce (Var index) = Var index
reduce UnitType = UnitType
reduce Unit = Unit


apply :: Expr -> Expr -> Expr
apply (Lambda t b) e = reduce (subst b e)
apply e1 e2 = Apply e1 e2


-- | Substitutes the second argument into the first assuming the first is a body
-- of a Lambda or Pi abstraction.
subst :: Expr -> Expr -> Expr
subst e1 e2 = subst' e1 e2 0
  where
    subst' u@(Universe level) _ _ = u
    subst'(Pi ta tb) e idx =
      let ta' = subst' ta e idx
          tb' = subst' tb (incIndices e) (idx + 1)
      in Pi ta' tb'
    subst'(Lambda t b) e idx =
      let t' = subst' t e idx
          b' = subst' b (incIndices e) (idx + 1)
      in Lambda t' b'
    subst'(Apply e1 e2) e idx =
      let e1' = subst' e1 e idx
          e2' = subst' e2 e idx
      in Apply e1' e2'
    subst' (Var index) e idx = if index == idx then e
                               else if index > idx then Var (pred index)
                               else Var index
    subst' UnitType _ _ = UnitType
    subst' Unit _ _ = Unit
