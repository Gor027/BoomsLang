-- Program to test parser, automatically generated by BNF Converter.

module Main where

import Grammar.Abs ()
import Grammar.Par (myLexer, pProgram)
import Grammar.Skel ()
import Processor.Evaluate
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

usage :: IO ()
usage = do
  putStrLn "Expecting input file as argument..."
  exitFailure

getInput :: [String] -> Maybe (IO String)
getInput [] = Nothing
getInput (fileName : _) = return $ readFile fileName

main :: IO ()
main = do
  args <- getArgs
  case getInput args of
    Nothing -> usage
    Just content -> do
      input <- content
      case pProgram $ myLexer input of
        Left msg -> do
          putStrLn "\nParse              Failed...\n"
          putStrLn "Tokens:"
          putStrLn msg
          exitFailure
        Right t -> do
          runIO t
