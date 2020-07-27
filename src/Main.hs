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


module Main where

import qualified AST as A
import Check
import Lexer
import Parser
import Reduce
import Representation
import Extraction.Untyped

import Control.Exception
import Data.Foldable (foldlM, foldrM)
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Parsec.Prim


main :: IO ()
main = do files <- getArgs
          binds <- loadFiles files
          seq binds (repl binds)


repl :: A.Bindings -> IO a
repl binds =
  do putStr "λ> "
     hFlush stdout

     eof <- hIsEOF stdin
     if eof then exitSuccess else return ()
     
     str <- getLine
     toks <- catch (evaluate (scan str)) (\e -> print (e :: ErrorCall) >> repl binds)
     if null toks then repl binds else
       case runParser topREPL () "REPL" toks of
         Left err -> putStrLn ("Parse Error: " ++ show err)

         Right (Left (A.Binding name ast)) ->
           case A.toTerm ast binds of
             Left err -> putStrLn ("Binding Error: " ++ show err)

             Right term ->
               case checkTerm term Star of
                 Left err -> putStrLn ("Type Error: " ++ ppError err)

                 Right typ ->
                   let e = reduceTerm term in
                     repl (A.addBinding name e binds)

         Right (Right ast) ->
           case A.toObject ast binds of
             Left err -> putStrLn ("Binding Error: " ++ show err)

             Right obj ->
               case checkObject obj Star of
                 Left err -> putStrLn ("Type Error: " ++ ppError err)

                 Right typ ->
                   let o = reduceObject obj in
                     do putStrLn $ "Value     | " ++ (ppObject o)
                        putStrLn $ "Type      | " ++ (ppTerm typ)
                        case extract (O o) of
                          Left err -> putStrLn ("Exraction Error: " ++ ppExtrError err)
                          Right Nothing -> return ()
                          Right (Just utlexpr) -> putStrLn $ "Extracted | " ++ (ppUntyped utlexpr)

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
          case checkTerm term Star of
            Left err -> error ("Type Error (" ++ file ++ "): " ++ ppError err)

            Right typ ->
              let t = reduceTerm term in
                A.addBinding name t binds
