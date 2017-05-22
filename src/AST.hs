
module AST where

import qualified Representation as R

import qualified Data.Map as M


newtype Bindings = Bindings { getBindMap :: M.Map String R.Expr }
  deriving (Eq, Show)

empty :: Bindings
empty = Bindings M.empty

addBinding :: String -> R.Expr -> Bindings -> Bindings
addBinding name expr (Bindings bs) = Bindings (M.insert name expr bs)

getBinding :: String -> Bindings -> Maybe R.Expr
getBinding name (Bindings bs) = M.lookup name bs


data BindingError = Undeclared String
  deriving (Eq, Show)


data Binding = Binding String Expr
  deriving (Eq, Show)

data Expr = Universe R.Level
          | Pi Expr Expr
          | Lambda Expr Expr
          | Apply Expr Expr
          | Var R.Index
          | UnitType
          | Unit
          | Bind String
          deriving (Eq, Show)


toRepr :: Expr -> Bindings -> Either BindingError R.Expr
toRepr (Universe l) _ = pure (R.Universe l)
toRepr (Pi arg body) binds = R.Pi <$> (toRepr arg binds) <*> (toRepr body binds)
toRepr (Lambda arg body) binds = R.Lambda <$>(toRepr arg binds) <*> (toRepr body binds)
toRepr (Apply e1 e2) binds = R.Apply <$> (toRepr e1 binds) <*> (toRepr e2 binds)
toRepr (Var index) _ = pure (R.Var index)
toRepr UnitType _ = pure R.UnitType
toRepr Unit _ = pure R.Unit
toRepr (Bind name) binds = case getBinding name binds of
                             Just expr -> pure expr
                             Nothing -> Left (Undeclared name)
