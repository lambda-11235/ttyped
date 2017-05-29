

module Main where

import qualified AST as A
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
main = do files <- getArgs
          binds <- loadFiles files
          seq binds (repl binds)


repl :: A.Bindings -> IO a
repl binds =
  do input <- RL.readline "λ> "
     case input of
       Nothing -> exitSuccess
       Just str ->
         do RL.addHistory str
            toks <- catch (evaluate (scan str)) (\e -> print (e :: ErrorCall) >> repl binds)
            if null toks then repl binds else
              case runParser topREPL () "REPL" toks of
                Left err -> putStrLn ("Parse Error: " ++ show err)

                Right (Left (A.Binding name ast)) ->
                  case A.toTerm ast binds of
                    Left err -> putStrLn ("Binding Error: " ++ show err)

                    Right term ->
                      case checkTerm term [] of
                        Left err -> putStrLn ("Type Error: " ++ ppError err)

                        Right typ ->
                          let e = reduceTerm term in
                            repl (A.addBinding name e binds)

                Right (Right ast) ->
                  case A.toTerm ast binds of
                    Left err -> putStrLn ("Binding Error: " ++ show err)

                    Right term ->
                      case checkTerm term [] of
                        Left err -> putStrLn ("Type Error: " ++ ppError err)

                        Right typ ->
                          let t = reduceTerm term in
                            do putStr (ppTerm t)
                               putStr " : "
                               putStrLn (ppTerm typ)

            repl binds


loadFiles :: [String] -> IO A.Bindings
loadFiles = foldlM loadFile A.empty

loadFile :: A.Bindings -> String -> IO A.Bindings
loadFile binds file = do contents <- readFile file
                         case runParser bindings () file (scan contents) of
                           Left err -> error (show err)
                           Right bs -> return (foldl add' binds bs)
  where
    add' binds (A.Binding name ast) =
      case A.toTerm ast binds of
        Left err -> error ("Binding Error (" ++ file ++ "): " ++ show err)

        Right term ->
          case checkTerm term [] of
            Left err -> error ("Type Error (" ++ file ++ "): " ++ ppError err)

            Right typ ->
              let t = reduceTerm term in
                A.addBinding name t binds
