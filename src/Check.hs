
module Check where

import Reduce
import Representation

import Data.Foldable (foldlM, foldrM)


data Error = VarNotInContext String Context
           | TypeMismatch Term Term
           | NonQuantTypeApplied Term Term
           deriving (Eq, Show)

ppError :: Error -> String
ppError (VarNotInContext name context) = "Variable " ++ name
  ++ " not in context " ++ ppContext context
ppError (TypeMismatch t1 t2) = "Got " ++ ppTerm t2 ++ " when expecting " ++ ppTerm t1
ppError (NonQuantTypeApplied t1 t2) = "Non quantified type " ++ ppTerm t1
  ++ " applied to " ++ ppTerm t2




checkTerm :: Term -> Context -> Either Error Term
checkTerm (C c) context = fmap C (checkContext c context)
checkTerm (O o) context = checkObject o context


-- Returns the first argument if there's no error.
checkContext :: Context -> Context -> Either Error Context
checkContext Star _ = return Star
checkContext (Quant name t c) context =
  do checkTerm t context
     checkContext c (concatTerm context name t)
     return (Quant name (reduceTerm t) (reduceContext c))


-- | Returns the type of the object passed in if there's no errors.
checkObject :: Object -> Context -> Either Error Term
checkObject (Var name index) context = asSeenFrom name index context
checkObject (Prod name t o) context =
  do checkTerm t context
     checkObject o (concatTerm context name (reduceTerm t))
     return (C Star)
checkObject (Fun name t o) context =
  do checkTerm t context
     let t' = reduceTerm t
     ot <- checkObject o (concatTerm context name t')
     case ot of
       (C c) -> return (C (Quant name t' (reduceContext c)))
       (O o) -> return (O (Prod name t' (reduceObject o)))
checkObject (App o1 o2) context =
  do o1t <- checkObject o1 context
     o2t <- checkObject o2 context
     checkTerm o1t context
     checkTerm o2t context
     o3 <- checkApply (reduceTerm o1t) (reduceTerm o2t) o2
     return (reduceTerm o3)


-- | Gets the type of a variable as seen from the context.
asSeenFrom :: String -> Nat -> Context -> Either Error Term
asSeenFrom name index context =
  do t <- getTerm index context (contextLength context)
     return (addTerm (index + 1) t)
  where
    getTerm _ Star _ = Left (VarNotInContext name context)
    getTerm index (Quant _ t c) len =
      if index == (len - 1) then return t
      else getTerm index c (len - 1)


-- | Returns the type of applying some type to another type.
checkApply :: Term -> Term -> Object -> Either Error Term
checkApply (C (Quant _ t1 c)) t2 o =
  if unify t1 t2 then return (C (substContext c o)) else Left (TypeMismatch t1 t2)
checkApply (O (Prod _ t1 o1)) t2 o2 =
  if unify t1 t2 then return (O (substObject o1 o2)) else Left (TypeMismatch t1 t2)
checkApply t1 t2 _ = Left (NonQuantTypeApplied t1 t2)


-- | Determines if two terms are the same. This is basically a test for equality
-- that ignores variable names.
unify :: Term -> Term -> Bool
unify (C c1) (C c2) = unifyContexts c1 c2
unify (O o1) (O o2) = unifyObjects o1 o2
unify _ _ = False

unifyContexts Star Star = True
unifyContexts (Quant _ t1 c1) (Quant _ t2 c2) = (unify t1 t2) && (unifyContexts c1 c2)
unifyContexts _ _ = False

unifyObjects (Var _ idx1) (Var _ idx2) = idx1 == idx2
unifyObjects (Prod _ t1 o1) (Prod _ t2 o2) = (unify t1 t2) && (unifyObjects o1 o2)
unifyObjects (Fun _ t1 o1) (Fun _ t2 o2) = (unify t1 t2) && (unifyObjects o1 o2)
unifyObjects (App o1 o2) (App o3 o4) = (unifyObjects o1 o3) && (unifyObjects o2 o4)
unifyObjects _ _ = False
