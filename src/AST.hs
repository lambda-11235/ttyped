
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


data Binding = Binding String AST
  deriving (Eq, Show)


data AST = Star
         | Var R.Nat
         | Prod AST AST
         | Fun AST AST
         | App AST AST
         | Bind String
         deriving (Eq, Show)


data ConversionError = Undeclared String
  deriving (Eq, Show)


toTerm :: AST -> Bindings -> Either ConversionError R.Term
toTerm Star _ = return R.Star
toTerm (Var index) _ = return (R.Var index)
toTerm (Fun t b) binds = R.Fun <$> (toTerm t binds) <*> (toTerm b binds)
toTerm (Prod t b) binds = R.Prod <$> (toTerm t binds) <*> (toTerm b binds)
toTerm (App t1 t2) binds = R.App <$> (toTerm t1 binds) <*> (toTerm t2 binds)
toTerm (Bind name) binds =
  case getBinding name binds of
    Nothing -> Left (Undeclared name)
    Just t -> return t
