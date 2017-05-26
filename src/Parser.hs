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

number :: Parser Nat
number = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' (LNumber n) = Just n
    match' _ = Nothing

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
topREPL :: Parser (Either Binding Object)
topREPL = (try (fmap Left binding) <|> fmap Right object) <* eof


bindings :: Parser [Binding]
bindings = many binding

binding :: Parser Binding
binding = do name <- sym
             match LEqual
             t <- term
             return (Binding name t)


term :: Parser Term
term = try (C <$> context) <|> (O <$> object)


context :: Parser Context
context = (match LStar *> pure Star) <|> quant <|> (CBind <$> sym)

quant = do match LLParen
           match LForall
           t <- term
           match LDot
           c <- context
           match LRParen
           return (Quant t c)


object :: Parser Object
object = parened <|> (Var <$> number) <|> (OBind <$> sym)

-- NOTE: This is necessary to avoid backtracking.
parened = do match LLParen
             e <- prod <|> fun <|> app
             match LRParen
             return e

prod = do match LForall
          t <- term
          match LDot
          o <- object
          return (Prod t o)

fun = do match LLambda
         t <- term
         match LDot
         o <- object
         return (Fun t o)

app = do o1 <- object
         o2 <- object
         return (App o1 o2)
