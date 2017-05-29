
module Check where

import Reduce
import Representation

import Data.Foldable (foldlM, foldrM)


type Env = [Expr]


data Error = NotAUniverse Expr
           | UnboundVar Index
           | NotSubType Expr Expr
           | NonPiTypeApplied Expr Expr
           | FinToLarge FinExpr
           | WrongNumCasesInFinElim FinExpr
           deriving (Eq, Show)


-- | Type check an expression, returning its type.
check :: Expr -> Either Error Expr
check e = check' e []


check' :: Expr -> Env -> Either Error Expr
check' (Universe level) _ = return (Universe (level + 1))
check' (Pi ta tb) env =
  do tat <- check' ta env
     let ta' = reduce ta
     tbt <- check' tb (map incIndices (ta' : env))
     maxUniverse tat tbt
check' (Lambda t b) env =
  do tt <- check' t env
     isUniverse tt
     let t' = reduce t
     bt <- check' b (map incIndices (t' : env))
     return (Pi t' bt)
check' (Apply e1 e2) env =
  do e1t <- check' e1 env
     e2t <- check' e2 env
     checkApply e1t e2t e2
check' (Var index) env = if (fromIntegral index) < (length env)
                            then return (env !! (fromIntegral index))
                         else Left (UnboundVar index)
check' (F finExpr) env = checkFinExpr finExpr env


checkFinExpr :: FinExpr -> Env -> Either Error Expr
checkFinExpr (FinType _) _ = return (Universe 0)
checkFinExpr f@(Fin n t) _ =
  if n < t then return (F (FinType t)) else Left (FinToLarge f)
checkFinExpr fe@(FinElim n l t cs fin) env =
  do tt <- check' t env
     subType tt (Pi (F (FinType n)) (Universe l))

     if length cs /= fromIntegral n
        then Left (WrongNumCasesInFinElim fe)
        else return ()
     csts <- mapM (\c -> check' c env) cs
     checkCases csts 0

     finT <- check' fin env
     subType finT (F (FinType n))

     return (reduce (Apply t fin))
  where
    checkCases [] _ = return ()
    checkCases (ct:csts) m =
      do let rt = reduce (Apply t (F (Fin m n)))
         check' rt env
         subType ct rt
         checkCases csts (m + 1)


maxUniverse :: Expr -> Expr -> Either Error Expr
maxUniverse (Universe n) (Universe m) = return (Universe (max n m))
maxUniverse (Universe _) e = Left (NotAUniverse e)
maxUniverse e _ = Left (NotAUniverse e)


isUniverse :: Expr -> Either Error ()
isUniverse (Universe n) = return ()
isUniverse e = Left (NotAUniverse e)


checkApply :: Expr -> Expr -> Expr -> Either Error Expr
checkApply (Pi ta tb) tc e =
  do subType tc ta
     return (reduce (subst tb e))
checkApply e1 e2 _ = Left (NonPiTypeApplied e1 e2)


-- | Determines if an element of a type given by the first argument can be used
-- where the type given by the second argument is expected.
subType :: Expr -> Expr -> Either Error ()
subType u1@(Universe n) u2@(Universe m) =
  if n <= m then return () else Left (NotSubType u1 u2)
subType (Pi t1 t2) (Pi t3 t4) = subType t3 t1 >> subType t2 t4
-- TODO: Determine if subtyping is correct.
subType t1 t2 = if t1 == t2 then return () else  Left (NotSubType t1 t2)
