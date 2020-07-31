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
import qualified Extraction.Scheme as ES

import qualified Options.Applicative as OA
import Data.Semigroup ((<>))
  
import Control.Exception
import Data.Foldable (foldlM, foldrM)
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Parsec.Prim


data CmdLine = CmdLine { getExtrDir :: Maybe String
                       , getFiles :: [String] }
  deriving (Show)

cmdLineP :: OA.Parser CmdLine
cmdLineP = CmdLine
  <$> OA.optional (OA.strOption
                   (OA.long "extract"
                     <> OA.short 'e'
                     <> OA.metavar "DIR"
                     <> OA.help "Extract Scheme code to DIR"))
  <*> OA.many (OA.strArgument (OA.metavar "FILE"))

cmdLineInfo :: OA.ParserInfo CmdLine
cmdLineInfo = OA.info (cmdLineP OA.<**> OA.helper)
  (OA.fullDesc
   <> OA.header "TTyped - An implementation of the Calculus of Constructions" )


main :: IO ()
main =
  do (CmdLine extrDir files) <- OA.execParser cmdLineInfo
     case extrDir of
       Nothing ->
         do (_, binds) <- loadFiles files
            seq binds (repl binds)
       Just dir -> extractFiles dir files


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


loadFiles :: [String] -> IO ([String], A.Bindings)
loadFiles files =
  do (vars, binds) <- foldlM loadFile ([], A.empty) files
     return (reverse vars, binds)

loadFile :: ([String], A.Bindings) -> String -> IO ([String], A.Bindings)
loadFile binds file = do contents <- readFile file
                         case runParser bindings () file (scan contents) of
                           Left err -> error (show err)
                           Right bs -> return (foldl add' binds bs)
  where
    add' (vars, binds) (A.Binding name ast) =
      case A.toTerm ast binds of
        Left err -> error ("Binding Error (" ++ file ++ "): " ++ show err)

        Right term ->
          case checkTerm term Star of
            Left err -> error ("Type Error (" ++ file ++ "): " ++ ppError err)

            Right typ ->
              let t = reduceTerm term in
                (name:vars, A.addBinding name t binds)


extractFiles :: String -> [String] -> IO ()
extractFiles = extractFiles' A.empty

extractFiles' :: A.Bindings -> String -> [String] -> IO ()
extractFiles' _ _ [] = return ()
extractFiles' binds dir (file:files) =
  do (vars, binds') <- loadFile ([], binds) file
     case ES.extractBindings (reverse vars) binds' of
       Left err -> putStrLn ("Exraction Error: " ++ ppExtrError err)
       Right s -> writeFile (outputPath dir file) (s ++ "\n")
     extractFiles' binds' dir files

outputPath :: String -> String -> String
outputPath dir file = dir ++ "/" ++ map replaceSlash file ++ ".scm"
  where
    replaceSlash '/' = '_'
    replaceSlash c = c
