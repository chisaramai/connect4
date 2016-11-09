module Main (main) where

import           Language.Haskell.HLint (hlint)
import           System.Exit            (exitFailure, exitSuccess)

arguments :: [String]
arguments = [ "benchmark", "app", "src", "test"]

main :: IO ()
main = do
        hints <- hlint $ "--utf8" : arguments
        if null hints 
        then exitSuccess 
        else exitFailure
