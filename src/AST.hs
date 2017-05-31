
module AST where

import qualified Representation as R

import Data.List (findIndex)
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


-- | The AST is basically just the context and object level squashed down into
-- one term language.
data AST = Star
         | Quant String AST AST
         | Var String
         | Fun String AST AST
         | App AST AST
         deriving (Eq, Show)


data ConversionError = NotAContext R.Object
                     | NotAObject R.Context
                     | Undeclared String
                     deriving (Eq, Show)


-- | Here we convert the AST to a Term. In this phase contexts are distinguished
-- from objects and Curry variables are replaced with de Bruijn indices.
toTerm :: AST -> Bindings -> Either ConversionError R.Term
toTerm ast binds = toTerm' ast binds []
  where
    toTerm' :: AST -> Bindings -> [String] -> Either ConversionError R.Term
    toTerm' Star _ _ = return (R.C R.Star)
    toTerm' (Quant name t b) binds vars =
      do t' <- toTerm' t binds vars
         b' <- toTerm' b binds (name:vars)
         case b' of
           (R.C c) -> return (R.C (R.Quant name t' c))
           (R.O o) -> return (R.O (R.Prod name t' o))
    toTerm' (Var name) binds vars =
      case findIndex (== name) vars of
        Just idx -> return (R.O (R.Var name (fromIntegral idx)))
        Nothing -> case getBinding name binds of
                     Just t -> return t
                     Nothing -> Left (Undeclared name)
    toTerm' (Fun name t b) binds vars =
      do b' <- toTerm' b binds (name:vars)
         R.O <$> ((R.Fun name) <$> (toTerm' t binds vars) <*> (assertObject b'))
    toTerm' (App o1 o2) binds vars =
      do o1' <- toTerm' o1 binds vars
         o2' <- toTerm' o2 binds vars
         R.O <$> (R.App <$> (assertObject o1') <*> (assertObject o2'))

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
