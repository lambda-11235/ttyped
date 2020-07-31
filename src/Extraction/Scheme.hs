
module Extraction.Scheme ( extract
                         , extractBindings
                         , convert )
where

import Data.List (intersperse)
import Data.Map ((!))

import AST (Bindings, getBindMap)
import Check
import qualified Extraction.Untyped as EU
import qualified Representation as R


extract :: R.Term -> Either EU.ExtrError (Maybe String)
extract t = do mexpr <- EU.extract t
               return $ fmap convert mexpr

convert :: EU.UntypedLC -> String
convert (EU.Var name _) = name
convert (EU.Fun name expr) = "(lambda (" ++ name ++ ") " ++ convert expr ++ ")"
convert (EU.App expr1 expr2) = "(" ++ convert expr1 ++ " " ++ convert expr2 ++ ")"
convert (EU.Axiom name) = name
convert (EU.Erased _) = "'()"
convert (EU.ErasedFun expr) = "(lambda (_) " ++ convert expr ++ ")"


-- | Extracts a set of variables given some bindings into Scheme code.
extractBindings :: [String] -> Bindings -> Either EU.ExtrError String
extractBindings vars bs =
  do let bs' = getBindMap bs
     ss <- mapM exBind (fmap (\v -> (v, bs' ! v)) vars)
     return $ concat $ intersperse "\n\n" ss
  where
    exBind (_, (R.O (R.Axiom name typ))) =
      return $ ";; Omitted axiom " ++ name ++ " : " ++ R.ppTerm typ
    exBind (name, expr) =
      do res <- extract expr
         case res of
           Nothing -> return $ ";; Type " ++ name ++ " = " ++ R.ppTerm expr
           Just s -> return $ "(define " ++ name ++ " " ++ s ++ ")"
