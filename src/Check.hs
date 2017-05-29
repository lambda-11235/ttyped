
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


-- | Returns the type of the term passed in if there's no errors.
checkTerm :: Term -> Context -> Either Error Term
checkTerm Star _ = return Star
checkTerm (Var index) context = asSeenFrom index context
checkTerm (Prod t1 t2) context =
  do checkTerm t1 context
     checkTerm t2 (insertTerm context (reduceTerm t1))
     return Star
checkTerm (Fun t1 t2) context =
  do checkTerm t1 context
     t2t <- checkTerm t2 (insertTerm context (reduceTerm t1))
     return (Prod (reduceTerm t1) t2t)
checkTerm (App t1 t2) context =
  do t1t <- checkTerm t1 context
     t2t <- checkTerm t2 context
     checkTerm t1t context
     checkTerm t2t context
     t3 <- checkApply (reduceTerm t1t) (reduceTerm t2t) t2
     return (reduceTerm t3)


-- | Gets the type of a variable as seen from the context.
asSeenFrom :: Nat -> Context -> Either Error Term
asSeenFrom index context =
  if index < contextLength context
  then return (addTerm (index + 1) (context !! (fromIntegral index)))
  else Left (VarNotInContext index context)


-- | Returns the type of applying some type to another type.
checkApply :: Term -> Term -> Term -> Either Error Term
checkApply (Prod t1 t2) t3 t4 =
  if t1 == t3 then return (substTerm t2 t4) else Left (TypeMismatch t1 t3)
checkApply t1 t2 _ = Left (NonQuantTypeApplied t1 t2)
