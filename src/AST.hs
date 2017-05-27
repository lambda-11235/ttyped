
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
         | Quant AST AST
         | Var R.Nat
         | Fun AST AST
         | App AST AST
         | Bind String
         deriving (Eq, Show)


data ConversionError = NotAContext R.Object
                     | NotAObject R.Context
                     | Undeclared String
                     deriving (Eq, Show)


toTerm :: AST -> Bindings -> Either ConversionError R.Term
toTerm Star _ = return (R.C R.Star)
toTerm (Quant t b) binds =
  do t' <- toTerm t binds
     b' <- toTerm b binds
     case b' of
       (R.C c) -> return (R.C (R.Quant t' c))
       (R.O o) -> return (R.O (R.Prod t' o))
toTerm (Var index) _ = return (R.O (R.Var index))
toTerm (Fun t b) binds =
  do b' <- toTerm b binds
     R.O <$> (R.Fun <$> (toTerm t binds) <*> (assertObject b'))
toTerm (App o1 o2) binds =
  do o1' <- toTerm o1 binds
     o2' <- toTerm o2 binds
     R.O <$> (R.App <$> (assertObject o1') <*> (assertObject o2'))
toTerm (Bind name) binds =
  case getBinding name binds of
    Nothing -> Left (Undeclared name)
    Just t -> return t

toContext :: AST -> Bindings -> Either ConversionError R.Context
toContext ast binds = toTerm ast binds >>= assertContext

toObject :: AST -> Bindings -> Either ConversionError R.Object
toObject ast binds = toTerm ast binds >>= assertObject


assertContext :: R.Term -> Either ConversionError R.Context
assertContext (R.C c) = return c
assertContext (R.O o) = Left (NotAContext o)

assertObject :: R.Term -> Either ConversionError R.Object
assertObject (R.C c) = Left (NotAObject c)
assertObject (R.O o) = return o
