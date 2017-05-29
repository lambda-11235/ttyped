
module Reduce ( reduceTerm
              , substTerm )
                where

import Representation

reduceTerm :: Term -> Term
reduceTerm Star = Star
reduceTerm v@(Var _) = v
reduceTerm (Prod t1 t2) = Prod (reduceTerm t1) (reduceTerm t2)
reduceTerm (Fun t1 t2) = Fun (reduceTerm t1) (reduceTerm t2)
reduceTerm (App t1 t2) = apply (reduceTerm t1) (reduceTerm t2)


apply :: Term -> Term -> Term
apply (Fun _ b) t = reduceTerm (substTerm b t)
apply t1 t2 = App t1 t2


substTerm :: Term -> Term -> Term
substTerm t1 t2 = substTerm' t1 t2 0


substTerm' Star _ _ = Star
substTerm' (Var index) t idx = if index == idx then t
                                 else if index > idx then Var (index - 1)
                                 else Var index
substTerm' (Prod t1 b) t2 idx =
  let t1' = substTerm' t1 t2 idx
      b' = substTerm' b (addTerm 1 t2) (idx + 1)
  in Prod t1' b'
substTerm' (Fun t1 b) t2 idx =
  let t1' = substTerm' t1 t2 idx
      b' = substTerm' b (addTerm 1 t2) (idx + 1)
  in Fun t1' b'
substTerm' (App t1 t2) t3 idx =
  let t1' = substTerm' t1 t3 idx
      t2' = substTerm' t2 t3 idx
  in App t1' t2'
