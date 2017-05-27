
module Reduce ( reduceTerm
              , reduceObject
              , substObject
              , substTerm
              , substContext)
                where

import Representation

reduceTerm :: Term -> Term
reduceTerm (C c) = C (mapContext reduceTerm c)
reduceTerm (O o) = O (reduceObject o)


reduceObject :: Object -> Object
reduceObject v@(Var _) = v
reduceObject (Prod t o) = Prod (reduceTerm t) (reduceObject o)
reduceObject (Fun t o) = Fun (reduceTerm t) (reduceObject o)
reduceObject (App o1 o2) = apply (reduceObject o1) (reduceObject o2)


apply :: Object -> Object -> Object
apply (Fun _ b) o = reduceObject (substObject b o)
apply o1 o2 = App o1 o2


substObject :: Object -> Object -> Object
substObject o1 o2 = substObject' o1 o2 0

substTerm :: Term -> Object -> Term
substTerm t o = substTerm' t o 0

substContext :: Context -> Object -> Context
substContext t o = substContext' t o 0


substObject' (Var index) o idx = if index == idx then o
                           else if index > idx then Var (index - 1)
                           else Var index
substObject' (Prod t b) o idx =
  let t' = substTerm' t o idx
      b' = substObject' b (addObject 1 o) (idx + 1)
  in Prod t' b'
substObject' (Fun t b) o idx =
  let t' = substTerm' t o idx
      b' = substObject' b (addObject 1 o) (idx + 1)
  in Fun t' b'
substObject' (App o1 o2) o3 idx =
  let o1' = substObject' o1 o3 idx
      o2' = substObject' o2 o3 idx
  in App o1' o2'

substTerm' (C c) o idx = C (substContext' c o idx)
substTerm' (O o1) o2 idx = O (substObject' o1 o2 idx)

substContext' Star o idx = Star
substContext' (Quant t c) o idx =
  Quant (substTerm' t o idx) (substContext' c (addObject 1 o) (idx + 1))
