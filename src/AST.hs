-- TTyped: A dependently typed programming language.
-- Copyright (C) 2018  Taran Lynn
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


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
         | Quant (Maybe String) AST AST
         | Var String
         | Fun (Maybe String) AST AST
         | App AST AST
         | Axiom String AST
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
         let name' = maybe R.unusedName id name
         b' <- toTerm' b binds (name':vars)
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
      do let name' = maybe R.unusedName id name
         b' <- toTerm' b binds (name':vars)
         R.O <$> ((R.Fun name) <$> (toTerm' t binds vars) <*> (assertObject b'))
    toTerm' (App o1 o2) binds vars =
      do o1' <- toTerm' o1 binds vars
         o2' <- toTerm' o2 binds vars
         R.O <$> (R.App <$> (assertObject o1') <*> (assertObject o2'))
    toTerm' (Axiom name t) binds vars =
      R.O <$> (R.Axiom name <$> (toTerm' t binds vars))

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
