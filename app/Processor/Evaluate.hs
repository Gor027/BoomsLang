module Processor.Evaluate where

import qualified Data.Map as M
import Grammar.Abs

runIO :: Program -> IO ()
runIO _ = do
  putStrLn "Success!"
