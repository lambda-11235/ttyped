-- TTyped: A dependently typed programming language.
-- Copyright (C) 2018  Taran Lynn
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


module Reduce ( reduceTerm
              , reduceObject
              , reduceContext
              , substObject
              , substTerm
              , substContext)
                where

import Representation


{- Note that all reductions assume the thing being reduced has been type
   checked.
-}


reduceTerm :: Term -> Term
reduceTerm (C c) = C (reduceContext c)
reduceTerm (O o) = O (reduceObject o)


reduceContext :: Context -> Context
reduceContext = mapContext reduceTerm


reduceObject :: Object -> Object
reduceObject v@(Var _ _) = v
reduceObject (Prod name t o) = Prod name (reduceTerm t) (reduceObject o)
reduceObject (Fun name t o) = Fun name (reduceTerm t) (reduceObject o)
reduceObject (App o1 o2) = apply (reduceObject o1) (reduceObject o2)


apply :: Object -> Object -> Object
apply (Fun _ _ b) o = reduceObject (substObject b o)
apply o1 o2 = App o1 o2


-- | Substitutes an object in for the top de Bruijn index 0 and decrements all
-- free variables.
substObject :: Object -> Object -> Object
substObject o1 o2 = substObject' o1 o2 0

substTerm :: Term -> Object -> Term
substTerm t o = substTerm' t o 0

substContext :: Context -> Object -> Context
substContext t o = substContext' t o 0


substObject' (Var name index) o idx =
  if index == idx then o
  else if index > idx then Var name (index - 1)
  else Var name index
substObject' (Prod name t b) o idx =
  let t' = substTerm' t o idx
      b' = substObject' b (addObject 1 o) (idx + 1)
  in Prod name t' b'
substObject' (Fun name t b) o idx =
  let t' = substTerm' t o idx
      b' = substObject' b (addObject 1 o) (idx + 1)
  in Fun name t' b'
substObject' (App o1 o2) o3 idx =
  let o1' = substObject' o1 o3 idx
      o2' = substObject' o2 o3 idx
  in App o1' o2'

substTerm' (C c) o idx = C (substContext' c o idx)
substTerm' (O o1) o2 idx = O (substObject' o1 o2 idx)

substContext' Star o idx = Star
substContext' (Quant name t c) o idx =
  Quant name (substTerm' t o idx) (substContext' c (addObject 1 o) (idx + 1))
