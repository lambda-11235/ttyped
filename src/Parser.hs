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


term = quant <|> fun <|> app

quant = do match LForall
           argAndBody Quant

fun = do match LLambda
         argAndBody Fun

argAndBody constructor =
  do name <- sym
     match LColon
     t <- explicit
     match LDot
     b <- term
     return (constructor name t b)

app = do o <- explicit
         os <- many explicit
         return (foldl App o os)


explicit = (match LStar *> pure Star) <|> (fmap Var sym) <|> parened

parened = do match LLParen
             e <- term
             match LRParen
             return e
