
module Extraction.Untyped ( UntypedLC (..)
                          , ExtrError (..)
                          , extract
                          , ppUntyped
                          , ppExtrError )
where

import Data.List (intersperse)

import Reduce (reduceObject)
import Representation (Nat)
import qualified Representation as R
import qualified Check as C


-- Type for terms in the untyped lambda calculus.
--
-- When extracted it should contain both valid symbolic variables, as
-- well as their de Bruijn indices. This is done to make conversion to
-- other languages easier.
--
-- Erased and ErasedFun are used to erase types. Extraction happens in
-- two steps.
--
-- 1. Any value whose type is a context is replased with Erased, and
-- expression of the form (\x : T. y) are replaced with (ErasedFun y).
--
-- 2. All instances of Erased and ErasedFun are removed.
data UntypedLC = Var String Nat
               | Fun String UntypedLC
               | App UntypedLC UntypedLC
               | Axiom String
               | Erased R.Object
               | ErasedFun UntypedLC

data ExtrError = TypeError C.Error
               | VarOutOfBound String Nat
               | TypeNotObject R.Object
               | CouldNotEraseObject R.Object


decExpr :: UntypedLC -> UntypedLC
decExpr = decExpr' 0

decExpr' :: Nat -> UntypedLC -> UntypedLC
decExpr' depth (Var name index) =
  if index < depth then Var name index else Var name (index - 1)
decExpr' depth (Fun name o) =
  Fun name (decExpr' (depth + 1) o)
decExpr' depth (App o1 o2) = App (decExpr' depth o1) (decExpr' depth o2)
decExpr' _ ax@(Axiom _) = ax
decExpr' _ (Erased o) = Erased o
decExpr' depth (ErasedFun o) = ErasedFun (decExpr' depth o)


-- Returns Left on error.
-- Returns Nothing if the term is a type.
extract :: R.Term -> Either ExtrError (Maybe UntypedLC)
extract t =
  case t of
    R.C _ -> return $ Nothing
    R.O obj ->
      case C.checkObject obj R.Star of
        Left err -> Left $ TypeError err
        Right (R.C _) -> return $ Nothing
        Right (R.O _) -> do expr <- erase [] R.Star (reduceObject obj)
                            expr' <- strip expr
                            return $ Just $ expr'


makeVar :: Nat -> Maybe String -> String
makeVar n Nothing = "v" ++ show n
makeVar n (Just s) = s ++ show n


-- Removes erased terms
strip :: UntypedLC -> Either ExtrError UntypedLC
strip var@(Var _ _) = return $ var
strip (Fun name o) =
  do o' <- strip o
     return $ Fun name o'
strip (App o1 (Erased _)) =  strip o1
strip (App o1 o2) =
  do o1' <- strip o1
     o2' <- strip o2
     return $ App o1' o2'
strip ax@(Axiom _) = return $ ax
strip (Erased o) = Left $ CouldNotEraseObject o
strip (ErasedFun o) = strip (decExpr o)


-- Erases all type level information from an object.
-- This will fail for any object whose type is a context.
erase :: [String] -> R.Context -> R.Object -> Either ExtrError UntypedLC
erase vars context (R.Var vname index) =
  if fromIntegral index < length vars then
    return $ Var (vars !! fromIntegral index) index
  else
    Left $ VarOutOfBound vname index
erase vars context prod@(R.Prod _ _ _) =
  Left $ TypeNotObject prod
erase vars context (R.Fun name t o) =
  let context' = R.concatTerm context name t
      depth = fromIntegral $ length vars
      name' = makeVar depth name
      vars' = name':vars
  in do o' <- erase vars' context' o
        case t of
          R.C _ -> return (ErasedFun o')
          R.O _ -> return $ Fun name' o'
erase vars context (R.App o1 o2) =
  do o1' <- erase vars context o1
     case C.checkObject o2 context of
       Left err -> Left $ TypeError err
       Right (R.C _) -> return $ App o1' (Erased o2)
       Right (R.O _) ->
         do o2' <- erase vars context o2
            return $ App o1' o2'
erase _ _ ax@(R.Axiom name t) =
  case t of
    R.C _ -> Left $ TypeNotObject ax
    R.O _ -> return $ Axiom name


maybeParen :: UntypedLC -> String
maybeParen expr@(Var _ _) = ppUntyped expr
maybeParen expr@(Axiom _) = ppUntyped expr
maybeParen expr = "(" ++ ppUntyped expr ++ ")"

ppUntyped :: UntypedLC -> String
ppUntyped (Var name _) = name
ppUntyped (Fun name expr) = ppFun [name] expr
ppUntyped (App expr1 expr2) = ppApps [expr2] expr1
ppUntyped (Axiom name) = name
ppUntyped (Erased _) = "<erased>"
ppUntyped (ErasedFun expr) = "λ<erased>. " ++ ppUntyped expr
  
ppFun :: [String] -> UntypedLC -> String
ppFun vars (Fun name expr) = ppFun (name:vars) expr
ppFun vars expr =
  let vars' = concat $ intersperse " " (reverse vars) in
    "λ" ++ vars' ++ ". " ++ ppUntyped expr

ppApps :: [UntypedLC] -> UntypedLC -> String
ppApps os (App o1 o2) = ppApps (o2:os) o1
ppApps os o =
  let os' = map maybeParen (o:os) in
    concat $ intersperse " " os'


ppExtrError :: ExtrError -> String
ppExtrError (TypeError err) = "Type error: " ++ C.ppError err
ppExtrError (VarOutOfBound name n) = "Variable out of bounds (" ++ name ++ "[" ++ show n ++ "])"
ppExtrError (TypeNotObject obj) = "Got type when expecting object (" ++ R.ppObject obj ++ ")"
ppExtrError (CouldNotEraseObject obj) = "Could not erase object (" ++ R.ppObject obj ++ ")"
