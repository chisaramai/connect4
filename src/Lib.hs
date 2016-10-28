module Lib
    ( showGrid
    , getChoice
    , dropToken
    , playWith
    ) where

import System.IO
import System.Environment
import Data.List
import Data.Char
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

showGrid :: [[Char]] -> IO()
showGrid grid = do
      putStrLn (intercalate "\n" ((transpose grid) -- show dots for every position in the grid
              ++ [filter isAlphaNum (unwords (map show [1..(length grid)]))] ++ [replicate 16 '-']))  -- show column numbers

--getChoice
getChoice from to = do
            hSetBuffering stdout NoBuffering
            putStr "Choose column: "
            choice <- getLine :: IO String -- no parse error
            if ((not(null choice)) && (isDigit (head choice)) && (from <= (read choice :: Int)) && (read choice :: Int) <= to)
                then
                return choice
                else
                getChoice from to

makeGrid :: Int -> Int -> [[Char]]
makeGrid m n       =  replicate m (replicate n '.') -- ^ sets up a m * n grid, filled with points at each position
                       

dropToken :: Char -> Int -> [[Char]] -> [[Char]]
dropToken token columnNumber grid =
                                         let columnIndex  = columnNumber - 1 -- ^ f.e. 1234 -> 0123
                                             originalColumn    = grid !! columnIndex -- ^ extract the chosen column
                                             emptyRows         = filter (== '.') originalColumn -- ^ scan how many free rows are left in that column
                                             columnPieces      = splitAt (length emptyRows) originalColumn -- ^ so it will look like this: (["..."],["XO"])
                                             changedColumn     = (init (fst columnPieces)) ++ (token : []) ++ (snd columnPieces) -- ^ insert the token at the end of rhe first List
                                             gridPieces        = splitAt columnNumber grid
                                             newGrid           = (init (fst gridPieces)) ++ (changedColumn : []) ++ (snd gridPieces)
                                         in newGrid
            
playWith :: [(Int,Char)] -> [[Char]] -> IO b
playWith  round grid = do
          choice <- getChoice 1 (length grid)
          putStrLn ("your choice: " ++ choice)
          let currentToken = head round
              changedGrid = (dropToken (snd currentToken) (read choice :: Int) grid)
              changedRound = (tail round) ++ (currentToken:[])
          showGrid changedGrid
          playWith changedRound changedGrid
  