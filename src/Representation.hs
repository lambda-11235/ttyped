
module Representation where

import Data.Word


type Nat = Word64


data Term = C Context
          | O Object
          deriving (Eq, Show)


data Context = Star
             | Quant Term Context
             deriving (Eq, Show)

contextLength :: Context -> Nat
contextLength Star = 0
contextLength (Quant _ c) = 1 + contextLength c

concatContext :: Context -> Context -> Context
concatContext Star c = c
concatContext (Quant t c1) c2 = Quant t (concatContext c1 c2)

concatTerm :: Context -> Term -> Context
concatTerm c t = concatContext c (Quant t Star)

mapContext :: (Term -> Term) -> Context -> Context
mapContext f Star = Star
mapContext f (Quant t c) = Quant (f t) (mapContext f c)


data Object = Var Nat
            | Prod Term Object
            | Fun Term Object
            | App Object Object
            deriving (Eq, Show)


addTerm :: Nat -> Term -> Term
addTerm n t = addTerm' n t 0

addContext :: Nat -> Context -> Context
addContext n t = addContext' n t 0

addObject :: Nat -> Object -> Object
addObject n t = addObject' n t 0


addTerm' n (C c) idx = C (addContext' n c idx)
addTerm' n (O o) idx = O (addObject' n o idx)


addContext' _ Star _ = Star
addContext' n (Quant t c) idx =
  Quant (addTerm' n t idx) (addContext' n c (idx + 1))


addObject' n (Var index) idx =
  if index < idx then Var index else Var (index + n)
addObject' n (Prod t o) idx =
  Prod (addTerm' n t idx) (addObject' n o (idx + 1))
addObject' n (Fun t o) idx =
  Fun (addTerm' n t idx) (addObject' n o (idx + 1))
addObject' n (App o1 o2) idx = App (addObject' n o1 idx) (addObject' n o2 idx)


ppTerm :: Term -> String
ppTerm (C c) = ppContext c
ppTerm (O o) = ppObject o

ppContext :: Context -> String
ppContext Star = "*"
ppContext (Quant t c) = "(∀" ++ ppTerm t ++ ". " ++ ppContext c ++ ")"

ppObject :: Object -> String
ppObject (Var index) = show index
ppObject (Prod t c) = "(∀" ++ ppTerm t ++ ". " ++ ppObject c ++ ")"
ppObject (Fun t c) = "(λ" ++ ppTerm t ++ ". " ++ ppObject c ++ ")"
ppObject (App o1 o2) = "(" ++ ppObject o1 ++ " " ++ ppObject o2 ++ ")"
