{
module Lexer (Token (..), LexOut (..), scan) where
}

%wrapper "posn"

@char = [a-zA-Z]
@digit = [0-9]

tokens :-

  $white+                               ;
  "#".*                                 ;

  "{"                                   { \p s -> lexOut p LLBracket }
  "}"                                   { \p s -> lexOut p LRBracket }

  "("                                   { \p s -> lexOut p LLParen }
  ")"                                   { \p s -> lexOut p LRParen }

  "["                                   { \p s -> lexOut p LLSquare }
  "]"                                   { \p s -> lexOut p LRSquare }

  ","                                   { \p s -> lexOut p LComma }
  "."                                   { \p s -> lexOut p LDot }

  "="                                   { \p s -> lexOut p LEqual }

  "F"                                   { \p s -> lexOut p LF }
  "finElim"                             { \p s -> lexOut p LFinElim }

  "||"                                  { \p s -> lexOut p LPi }
  "Π"                                   { \p s -> lexOut p LPi }

  "\"                                   { \p s -> lexOut p LLambda }
  "λ"                                   { \p s -> lexOut p LLambda }

  "*"                                   { \p s -> lexOut p LStar }

  @digit+                               { \p s -> lexOut p (LNumber (read s)) }
  @char+                                { \p s -> lexOut p (LSym s) }

{
data Token = LLBracket
           | LRBracket
           | LLParen
           | LRParen
           | LLSquare
           | LRSquare
           | LComma
           | LDot
           | LEqual
           | LF
           | LFinElim
           | LLambda
           | LPi
           | LStar
           | LNumber Int
           | LSym String
           deriving (Eq, Show)

data LexOut = LexOut { offset :: Int
                     , line :: Int
                     , column :: Int
                     , getToken :: Token }
              deriving (Eq, Show)

lexOut (AlexPn offset line col) tok = LexOut offset line col tok

scan = alexScanTokens
}
