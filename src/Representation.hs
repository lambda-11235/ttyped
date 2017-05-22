
module Representation ( Level
                      , Index
                      , Expr (..)
                      , incIndices
                      , ppExpr)
  where

import Data.Word


type Level = Word8
type Index = Word8


data Expr = Universe Level
          | Pi Expr Expr
          | Lambda Expr Expr
          | Apply Expr Expr
          | Var Index
          | UnitType
          | Unit
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
    incIndices' UnitType _ = UnitType
    incIndices' Unit _ = Unit


ppExpr :: Expr -> String
ppExpr (Universe level) =
  '*' : (if level == 0 then "" else "{" ++ (show level) ++ "}")
ppExpr (Pi arg body) = "(Π " ++ (ppExpr arg) ++ ". " ++ (ppExpr body) ++ ")"
ppExpr (Lambda arg body) = "(λ " ++ (ppExpr arg) ++ ". " ++ (ppExpr body) ++ ")"
ppExpr (Apply e1 e2) = "(" ++ (ppExpr e1) ++ " " ++ (ppExpr e2) ++ ")"
ppExpr (Var index) = show index
ppExpr UnitType = "ut"
ppExpr Unit = "u"
