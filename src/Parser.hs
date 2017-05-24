module Parser where

import AST
import Lexer

import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim


type Parser = Parsec [LexOut] ()


match :: Token -> Parser ()
match tok = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' x = if x == tok then Just () else Nothing

number :: Parser Int
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


-- | A top level entry in the REPL.
topREPL :: Parser (Either Binding Expr)
topREPL = (try (fmap Left binding) <|> fmap Right expr) <* eof


bindings :: Parser [Binding]
bindings = many binding

binding :: Parser Binding
binding = do name <- sym
             match LEqual
             e <- expr
             return (Binding name e)


expr :: Parser Expr
expr = universe <|> parened <|> (fmap (Var . fromIntegral) number)
  <|> (fmap F finExpr)
  <|> (fmap Bind sym)

universe = do match LStar
              ml <- optionMaybe level
              case ml of
                Just l -> return (Universe l)
                Nothing -> return (Universe 0)
  where
    level = do match LLBracket
               l <- number
               match LRBracket
               return (fromIntegral l)

-- NOTE: This is necessary to avoid backtracking.
parened = do match LLParen
             e <- piP <|> lambda <|> apply
             match LRParen
             return e

piP = do match LPi
         argTyp <- expr
         match LDot
         body <- expr
         return (Pi argTyp body)

lambda = do match LLambda
            argTyp <- expr
            match LDot
            body <- expr
            return (Lambda argTyp body)

apply = do e1 <- expr
           e2 <- expr
           return (Apply e1 e2)


finExpr :: Parser FinExpr
finExpr = finType <|> fin <|> finElim

finType = do match LF
             match LLSquare
             n <- fmap fromIntegral number
             match LRSquare
             return (FinType n)

fin= do match LLSquare
        n <- fmap fromIntegral number
        match LComma
        t <- fmap fromIntegral number
        match LRSquare
        return (Fin n t)

finElim = do match LFinElim
             match LLSquare
             n <- fmap fromIntegral number
             match LRSquare
             return (FinElim n)
