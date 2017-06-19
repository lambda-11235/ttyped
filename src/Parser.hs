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
-- TODO: removes try's.

-- | A top level entry in the REPL.
topREPL :: Parser (Either Binding AST)
topREPL = (try (fmap Left binding) <|> fmap Right ast) <* eof


bindings :: Parser [Binding]
bindings = many binding <* eof

binding :: Parser Binding
binding = do name <- sym
             match LEqual
             t <- ast
             return (Binding name t)


ast :: Parser AST
ast = (match LStar *> pure Star) <|> parened <|> (Var <$> sym)

-- NOTE: This is necessary to avoid backtracking.
parened = do match LLParen
             e <- quant <|> fun <|> app
             match LRParen
             return e

quant = do match LForall
           name <- sym
           match LColon
           t <- ast
           match LDot
           b <- ast
           return (Quant name t b)

fun = do match LLambda
         name <- sym
         match LColon
         t <- ast
         match LDot
         b <- ast
         return (Fun name t b)

app = do o <- ast
         os <- many1 ast
         return (foldl App o os)
