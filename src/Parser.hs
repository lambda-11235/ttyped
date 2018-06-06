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


module Parser where

import AST
import Lexer
import Representation (Nat)

import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim


type Parser = Parsec [LexOut] ()


match :: Token -> Parser ()
match tok = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' x = if x == tok then Just () else Nothing

{-number :: Parser Nat
number = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' (LNumber n) = Just n
    match' _ = Nothing-}

sym :: Parser String
sym = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' (LSym name) = Just name
    match' _ = Nothing

pos :: (SourcePos -> LexOut -> [LexOut] -> SourcePos)
pos oldPos (LexOut _ line col _) _ = newPos (sourceName oldPos) line col


-- * Grammar

-- | A top level entry in the REPL.
topREPL :: Parser (Either Binding AST)
topREPL = (fmap Left binding <|> fmap Right ast) <* eof


bindings :: Parser [Binding]
bindings = many binding <* eof

binding :: Parser Binding
binding = do match LLet
             name <- sym
             match LEqual
             t <- ast
             return (Binding name t)


ast :: Parser AST
ast = term


term = quant <|> fun <|> funTypeOrApp

quant = do match LForall
           argAndBody Quant

fun = do match LLambda
         argAndBody Fun

argAndBody constructor =
  do name <- (match LUnderscore *> return Nothing) <|> (fmap Just sym)
     match LColon
     t <- explicit
     match LDot
     b <- term
     return (constructor name t b)

funTypeOrApp =
  do x <- explicit
     xs <- fmap Left (many1 (match LArrow *> explicit)) <|> fmap Right (many explicit)
     case xs of
       Left ts -> return (foldr1 (Quant Nothing) (x:ts))
       Right os ->return (foldl App x os)


explicit = (match LStar *> pure Star) <|> (fmap Var sym) <|> parened

parened = do match LLParen
             e <- term
             match LRParen
             return e
