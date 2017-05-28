
module Check where

import Reduce
import Representation

import Data.Foldable (foldlM, foldrM)


data Error = VarNotInContext Nat Context
           | TypeMismatch Term Term
           | NonQuantTypeApplied Term Term
           deriving (Eq, Show)

ppError :: Error -> String
ppError (VarNotInContext index context) = "Variable " ++ show index
  ++ " not in context " ++ ppContext context
ppError (TypeMismatch t1 t2) = "Got " ++ ppTerm t2 ++ " when expecting " ++ ppTerm t1
ppError (NonQuantTypeApplied t1 t2) = "Non quantified type " ++ ppTerm t1
  ++ " applied to " ++ ppTerm t2




checkTerm :: Term -> Context -> Either Error Term
checkTerm (C c) context = fmap C (checkContext c context)
checkTerm (O o) context = checkObject o context


-- Returns itself if there's no error.
checkContext :: Context -> Context -> Either Error Context
checkContext Star _ = return Star
checkContext (Quant t c) context =
  do checkTerm t context
     checkContext c (concatTerm context t)
     return (Quant (reduceTerm t) (reduceContext c))


checkObject :: Object -> Context -> Either Error Term
checkObject (Var index) context = asSeenFrom index context
checkObject (Prod t o) context =
  do checkTerm t context
     checkObject o (concatTerm context t)
     return (C Star)
checkObject (Fun t o) context =
  do checkTerm t context
     ot <- checkObject o (concatTerm context t)
     let t' = reduceTerm t
     case ot of
       (C c) -> return (C (Quant t' (reduceContext c)))
       (O o) -> return (O (Prod t' (reduceObject o)))
checkObject (App o1 o2) context =
  do o1t <- checkObject o1 context
     o2t <- checkObject o2 context
     checkTerm o1t context
     checkTerm o2t context
     o3 <- checkApply (reduceTerm o1t) (reduceTerm o2t) o2
     return (reduceTerm o3)


asSeenFrom :: Nat -> Context -> Either Error Term
asSeenFrom index context =
  do t <- getTerm index context (contextLength context)
     return (addTerm (index + 1) t)
  where
    getTerm _ Star _ = Left (VarNotInContext index context)
    getTerm index (Quant t c) len =
      if index == (len - 1) then return t
      else getTerm index c (len - 1)


checkApply :: Term -> Term -> Object -> Either Error Term
checkApply (C (Quant t1 c)) t2 o =
  if t1 == t2 then return (C (substContext c o)) else Left (TypeMismatch t1 t2)
checkApply (O (Prod t1 o1)) t2 o2 =
  if t1 == t2 then return (O (substObject o1 o2)) else Left (TypeMismatch t1 t2)
checkApply t1 t2 _ = Left (NonQuantTypeApplied t1 t2)
