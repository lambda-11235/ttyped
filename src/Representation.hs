
module Representation where

import Data.List (intersperse)
import Data.Word


type Nat = Word64


{- The strings in Quant, Var, Prod, and Fun all represent variable names.
   However, since variables use de Bruijn indices, these variable names are only
   used for debugging purposes.
-}


data Term = C Context
          | O Object
          deriving (Eq, Show)


data Context = Star
             | Quant String Term Context
             deriving (Eq, Show)

contextLength :: Context -> Nat
contextLength Star = 0
contextLength (Quant _ _ c) = 1 + contextLength c

concatContext :: Context -> Context -> Context
concatContext Star c = c
concatContext (Quant name t c1) c2 = Quant name t (concatContext c1 c2)

concatTerm :: Context -> String -> Term -> Context
concatTerm c name t = concatContext c (Quant name t Star)

mapContext :: (Term -> Term) -> Context -> Context
mapContext f Star = Star
mapContext f (Quant name t c) = Quant name (f t) (mapContext f c)


data Object = Var String Nat
            | Prod String Term Object
            | Fun String Term Object
            | App Object Object
            deriving (Eq, Show)


-- | Increments all free variables by some amount.
addTerm :: Nat -> Term -> Term
addTerm n t = addTerm' n t 0

addContext :: Nat -> Context -> Context
addContext n t = addContext' n t 0

addObject :: Nat -> Object -> Object
addObject n t = addObject' n t 0


addTerm' n (C c) idx = C (addContext' n c idx)
addTerm' n (O o) idx = O (addObject' n o idx)


addContext' _ Star _ = Star
addContext' n (Quant name t c) idx =
  Quant name (addTerm' n t idx) (addContext' n c (idx + 1))


addObject' n (Var name index) idx =
  if index < idx then Var name index else Var name (index + n)
addObject' n (Prod name t o) idx =
  Prod name (addTerm' n t idx) (addObject' n o (idx + 1))
addObject' n (Fun name t o) idx =
  Fun name (addTerm' n t idx) (addObject' n o (idx + 1))
addObject' n (App o1 o2) idx = App (addObject' n o1 idx) (addObject' n o2 idx)


ppTerm :: Term -> String
ppTerm (C c) = ppContext c
ppTerm (O o) = ppObject o

ppContext :: Context -> String
ppContext Star = "*"
ppContext (Quant name t c) =
  "(∀" ++ name ++ " : " ++ ppTerm t ++ ". " ++ ppContext c ++ ")"

ppObject :: Object -> String
ppObject (Var name index) = name ++ "[" ++ show index ++ "]"
ppObject (Prod name t c) = "(∀" ++ name ++ " : " ++ ppTerm t ++ ". " ++ ppObject c ++ ")"
ppObject (Fun name t c) = "(λ" ++ name ++ " : " ++ ppTerm t ++ ". " ++ ppObject c ++ ")"
ppObject (App o1 o2) = ppApps [o2] o1

ppApps os (App o1 o2) = ppApps (o2:os) o1
ppApps os o = ppApps' (o:os)
  where
    ppApps' os = "(" ++ (intersperse " " (map ppObject os) >>= id) ++ ")"
