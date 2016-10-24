module Lib
    ( showGrid
    , getChoice
    ) where

import System.IO
import System.Environment
import Data.List
import Data.Char
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

--showGrid column row :: IO()
showGrid m n = do
      putStrLn ( intercalate "\n" ((replicate m (replicate n '.')) -- show dots for every position in the grid
              ++ [filter isAlphaNum (unwords (map show [1..n]))])) -- show column numbers

--getChoice
getChoice = do
            hSetBuffering stdout NoBuffering
            putStr "Choose column: "
            choice <- getLine
            if (isDigit (read(choice)))
              then return choice
              else getChoice
