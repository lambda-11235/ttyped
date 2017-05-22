
module Main where

import Check
import Lexer
import Parser
import Reduce
import Representation

import Control.Exception
import Data.Foldable (foldlM, foldrM)
import qualified System.Console.Readline as RL
import System.Environment (getArgs)
import System.Exit
import Text.Parsec.Prim


main :: IO ()
main = repl


repl :: IO a
repl =
  do input <- RL.readline "λ> "
     case input of
       Nothing -> exitSuccess
       Just str ->
         do RL.addHistory str
            toks <- catch (evaluate (scan str)) (\e -> print (e :: ErrorCall) >> repl)
            if null toks then repl else
              case runParser topREPL () "REPL" toks of
                Left err -> putStrLn ("Parse Error: " ++ show err)

                Right expr ->
                  case check expr of
                    Left err -> putStrLn ("Type Error: " ++ show err)

                    Right typ ->
                      let e = reduce expr in
                        do putStr (ppExpr e)
                           putStr " : "
                           putStrLn (ppExpr typ)
            repl
