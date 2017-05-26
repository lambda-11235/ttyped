
module AST where

import qualified Representation as R

import qualified Data.Map as M


newtype Bindings = Bindings { getBindMap :: M.Map String R.Term }
  deriving (Eq, Show)

empty :: Bindings
empty = Bindings M.empty

addBinding :: String -> R.Term -> Bindings -> Bindings
addBinding name term (Bindings bs) = Bindings (M.insert name term bs)

getBinding :: String -> Bindings -> Maybe R.Term
getBinding name (Bindings bs) = M.lookup name bs


data BindingError = NotAContext String R.Object
                  | NotAObject String R.Context
                  | Undeclared String
  deriving (Eq, Show)


data Binding = Binding String Term
  deriving (Eq, Show)


data Term = C Context
          | O Object
          deriving (Eq, Show)

data Context = Star
             | Quant Term Context
             | CBind String
             deriving (Eq, Show)

data Object = Var R.Nat
            | Prod Term Object
            | Fun Term Object
            | App Object Object
            | OBind String
            deriving (Eq, Show)


toTerm :: Term -> Bindings -> Either BindingError R.Term
toTerm (C (CBind name)) binds =
  maybe (Left (Undeclared name)) Right (getBinding name binds)
toTerm (O (OBind name)) binds =
  maybe (Left (Undeclared name)) Right (getBinding name binds)
toTerm (C c) binds = R.C <$> (toContext c binds)
toTerm (O o) binds = R.O <$> (toObject o binds)

toContext :: Context -> Bindings -> Either BindingError R.Context
toContext Star _ = pure R.Star
toContext (Quant t c) binds = R.Quant <$> (toTerm t binds) <*> (toContext c binds)
toContext (CBind name) binds =
  case getBinding name binds of
    Nothing -> Left (Undeclared name)
    Just (R.C c) -> pure c
    Just (R.O o) -> Left (NotAContext name o)

toObject :: Object -> Bindings -> Either BindingError R.Object
toObject (Var index) _ = pure (R.Var index)
toObject (Prod t b) binds = R.Prod <$> (toTerm t binds) <*> (toObject b binds)
toObject (Fun t b) binds = R.Fun <$> (toTerm t binds) <*> (toObject b binds)
toObject (App o1 o2) binds = R.App <$> (toObject o1 binds) <*> (toObject o2 binds)
toObject (OBind name) binds =
  case getBinding name binds of
    Nothing -> Left (Undeclared name)
    Just (R.C c) -> Left (NotAObject name c)
    Just (R.O o) -> pure o
