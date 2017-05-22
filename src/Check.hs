
module Check where

import Reduce
import Representation

type Env = [Expr]


data Error = NotAUniverse Expr
           | UnboundVar Index
           | NoUnify Expr Expr
           | NonPiTypeApplied Expr Expr
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
  do check' t env
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
check' UnitType _ = return (Universe 0)
check' Unit _ = return UnitType


maxUniverse :: Expr -> Expr -> Either Error Expr
maxUniverse (Universe n) (Universe m) = pure (Universe (max n m))
maxUniverse (Universe _) e = Left (NotAUniverse e)
maxUniverse e _ = Left (NotAUniverse e)

checkApply :: Expr -> Expr -> Expr -> Either Error Expr
checkApply (Pi ta tb) tc e =
  do unify tc ta
     return (reduce (subst tb e))
checkApply e1 e2 _ = Left (NonPiTypeApplied e1 e2)

-- | Determines if an element of a type given by the first argument can be
-- used where the type given by the second argument is expected.
unify :: Expr -> Expr -> Either Error ()
unify u1@(Universe n) u2@(Universe m) =
  if n <= m then return () else Left (NoUnify u1 u2)
unify (Pi t1 t2) (Pi t3 t4) =
  do unify t1 t3
     unify t2 t4
unify (Lambda t1 b1) (Lambda t2 b2) =
  do unify t1 t2
     unify b1 b2
unify (Apply e1 e2) (Apply e3 e4) =
  do unify e1 e3
     unify e2 e4
unify v1@(Var n) v2@(Var m) =
  if n == m then return () else Left (NoUnify v1 v2)
unify UnitType UnitType = return ()
unify Unit Unit = return ()
unify t1 t2 = Left (NoUnify t1 t2)
