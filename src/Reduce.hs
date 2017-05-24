
module Reduce (reduce, subst) where

import Representation

-- | Reduces a type checked expression to normal form.
reduce :: Expr -> Expr
reduce (Universe level) = Universe level
reduce (Pi ta tb) = Pi (reduce ta) (reduce tb)
reduce (Lambda t b) = Lambda (reduce t) (reduce b)
reduce (Apply e1 e2) = apply (reduce e1) (reduce e2)
reduce (Var index) = Var index
reduce fe@(F _) = fe


apply :: Expr -> Expr -> Expr
apply (Lambda t b) e = reduce (subst b e)
apply (F finExpr) e = applyFinExpr finExpr e
apply e1 e2 = Apply e1 e2

applyFinExpr :: FinExpr -> Expr -> Expr
applyFinExpr (FinElim n Nothing) t = F (FinElim n (Just (t, [])))
applyFinExpr fe@(FinElim n (Just (t, cs))) e =
  if length cs < fromIntegral n then F (FinElim n (Just (t, cs ++ [e]))) else
  case e of
    (F (Fin m _)) -> cs !! (fromIntegral m)
    _ -> Apply (F fe) e
applyFinExpr fe e = Apply (F fe) e


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
    subst' (F finExpr) e idx = F (substFin finExpr e idx)

    substFin ft@(FinType _) _ _ = ft
    substFin f@(Fin _ _) _ _ = f
    substFin fe@(FinElim n Nothing) _ _ = fe
    substFin (FinElim n (Just (t, cs))) e idx =
      FinElim n (Just (subst' t e idx, map (\c -> subst' c e idx) cs))
