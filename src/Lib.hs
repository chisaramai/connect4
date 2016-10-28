module Lib
    ( showGrid
    , getChoiceOf
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

--getChoiceOf :: Int -> Int -> Int
getChoiceOf token from to = do
            hSetBuffering stdout NoBuffering
            putStr "Choose column: "
            choice <- getLine :: IO String
            if ((not(null choice)) && (isDigit (head choice)) && (from <= (read choice :: Int)) && (read choice :: Int) <= to)
                then
                return (read choice :: Int)
                else
                getChoiceOf token from to

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
          let currentToken = head round
          choice <- getChoiceOf currentToken 1 (length grid)
          let changedGrid = (dropToken (snd currentToken) choice grid)
              changedRound = (tail round) ++ (currentToken:[])
          showGrid changedGrid
          playWith changedRound changedGrid
  