
module Representation where

import Data.Word


type Nat = Word64


type Context = [Term]

contextLength :: Context -> Nat
contextLength ctx = fromIntegral $ length ctx

insertTerm :: Context -> Term -> Context
insertTerm c t = t : c


data Term = Star
          | Var Nat
          | Prod Term Term
          | Fun Term Term
          | App Term Term
          deriving (Eq, Show)


addTerm :: Nat -> Term -> Term
addTerm n t = addTerm' n t 0
  where
    addTerm' _ Star _ = Star
    addTerm' n (Var index) idx =
      if index < idx then Var index else Var (index + n)
    addTerm' n (Prod t1 t) idx =
      Prod (addTerm' n t1 idx) (addTerm' n t (idx + 1))
    addTerm' n (Fun t1 t) idx =
      Fun (addTerm' n t1 idx) (addTerm' n t (idx + 1))
    addTerm' n (App t1 t2) idx = App (addTerm' n t1 idx) (addTerm' n t2 idx)


ppTerm :: Term -> String
ppTerm Star = "*"
ppTerm (Var index) = show index
ppTerm (Prod t1 t2) = "(∀" ++ ppTerm t1 ++ ". " ++ ppTerm t2 ++ ")"
ppTerm (Fun t1 t2) = "(λ" ++ ppTerm t1 ++ ". " ++ ppTerm t2 ++ ")"
ppTerm (App t1 t2) = "(" ++ ppTerm t1 ++ " " ++ ppTerm t2 ++ ")"

ppContext :: Context -> String
ppContext ctx = "{" ++ ppc ctx 0 ++ "}"
  where
    ppc [] _ = ""
    ppc [t] n = show n ++ " = " ++ ppTerm t
    ppc (t:ts) n = show n ++ " = " ++ ppTerm t ++ ", " ++ ppc ts (n + 1)
