
module Representation ( Level
                      , Index
                      , Nat
                      , Expr (..)
                      , FinExpr (..)
                      , incIndices
                      , ppExpr)
  where

import Data.Word


type Level = Word64
type Index = Word64
type Nat = Word64


data Expr = Universe Level
          | Pi Expr Expr
          | Lambda Expr Expr
          | Apply Expr Expr
          | Var Index
          | F FinExpr
          deriving (Eq, Show)

data FinExpr = FinType Nat
             | Fin Nat Nat
             -- FinElim last argument is optionally the type and cases it's
             -- applied to.
             | FinElim Nat Level Expr [Expr] Expr
             deriving (Eq, Show)


incIndices :: Expr -> Expr
incIndices e = incIndices' e 0
  where
    incIndices' u@(Universe _) _ = u
    incIndices' (Pi arg body) idx =
      Pi (incIndices' arg idx) (incIndices' body (idx + 1))
    incIndices' (Lambda arg body) idx =
      Lambda (incIndices' arg idx) (incIndices' body (idx + 1))
    incIndices' (Apply e1 e2) idx =
      Apply (incIndices' e1 idx) (incIndices' e2 idx)
    incIndices' (Var index) idx =
      if index < idx then Var index else Var (index + 1)
    incIndices' (F finExpr) idx = F (incFinExpr finExpr idx)

    incFinExpr ft@(FinType _) _ = ft
    incFinExpr f@(Fin _ _) _ = f
    incFinExpr (FinElim n l t cs fin) idx =
      let ii x = incIndices' x idx
          t' = ii t
          cs' = map ii cs
       in FinElim n l t' cs' fin


ppExpr :: Expr -> String
ppExpr (Universe level) =
  '*' : (if level == 0 then "" else "{" ++ (show level) ++ "}")
ppExpr (Pi arg body) = "(Π " ++ (ppExpr arg) ++ ". " ++ (ppExpr body) ++ ")"
ppExpr (Lambda arg body) = "(λ " ++ (ppExpr arg) ++ ". " ++ (ppExpr body) ++ ")"
ppExpr (Apply e1 e2) = "(" ++ (ppExpr e1) ++ " " ++ (ppExpr e2) ++ ")"
ppExpr (Var index) = show index
ppExpr (F finExpr) = ppFinExpr finExpr

ppFinExpr (FinType n) = "F[" ++ show n ++ "]"
ppFinExpr (Fin n t) = "[" ++ show n ++ "," ++ show t ++ "]"
ppFinExpr (FinElim n l t cs fin) =
  "finElim[" ++ show n ++ "," ++ show l ++ "](" ++ ppArgs (t : (cs ++ [fin])) ++ ")"
  where
    ppArgs [] = ""
    ppArgs [x] = ppExpr x
    ppArgs (x:xs) = ppExpr x ++ ", " ++ ppArgs xs
