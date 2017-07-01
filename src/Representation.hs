
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
ppTerm = ppTerm' [] False

ppContext :: Context -> String
ppContext = ppContext' [] False

ppObject :: Object -> String
ppObject = ppObject' [] False

{-
   For all the following functions the first argument is a list of bound
   varibles, and the second argument determines whether the term should be
   explicitly parenthesized.
-}

ppTerm' :: [String] -> Bool -> Term -> String
ppTerm' vars expParen (C c) = ppContext' vars expParen c
ppTerm' vars expParen (O o) = ppObject' vars expParen o

ppContext' :: [String] -> Bool -> Context -> String
ppContext' _ _ Star = "*"
ppContext' vars expParen (Quant name t c) =
  let s = "∀" ++ name ++ " : " ++ ppTerm' vars True t ++ ". " ++ ppContext' (name:vars) False c
   in maybeParen expParen s

ppObject' :: [String] -> Bool -> Object -> String
ppObject' vars _ (Var name index) =
  if length (filter (== name) vars) > 1
  then name ++ "[" ++ show index ++ "]"
  else name
ppObject' vars expParen (Prod name t c) =
  let s = "∀" ++ name ++ " : " ++ ppTerm' vars True t ++ ". " ++ ppObject' (name:vars) False c
   in maybeParen expParen s
ppObject' vars expParen (Fun name t o) =
  let s = "λ" ++ name ++ " : " ++ ppTerm' vars True t ++ ". " ++ ppObject' (name:vars) False o
   in maybeParen expParen s
ppObject' vars expParen (App o1 o2) = ppApps vars expParen [o2] o1

maybeParen :: Bool -> String -> String
maybeParen False s = s
maybeParen True s = "(" ++ s ++ ")"

ppApps vars expParen os (App o1 o2) = ppApps vars expParen (o2:os) o1
ppApps vars expParen os o = ppApps' (o:os)
  where
    ppApps' os = maybeParen expParen (intersperse " " (map (ppObject' vars True) os) >>= id)
