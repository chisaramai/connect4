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
getChoice = do
            hSetBuffering stdout NoBuffering
            putStr "Choose column: "
            choice <- getLine :: IO String -- no parse error
            if ((choice /= []) && not (isDigit (head choice)) && (0 /= (length choice) ))
                then
                getChoice
                else
                return choice

makeGrid :: Int -> Int -> [[Char]]
makeGrid m n       =  replicate m (replicate n '.') -- ^ sets up a m * n grid, filled with points at each position
                       

dropToken :: Char -> Int -> [[Char]] -> [[Char]]
dropToken token columnNumber grid 
            | columnNumber <= (length (head grid)) = let columnIndex       = columnNumber - 1 -- ^ f.e. 1234 -> 0123
                                                         originalColumn    = grid !! columnIndex -- ^ extract the chosen column
                                                         numberOfEmptyRows = filter (== '.') originalColumn -- ^ scan how many free rows are left in that column
                                                         columnPieces      = splitAt (length numberOfEmptyRows) originalColumn -- ^ so it will look like this: (["..."],["XO"])
                                                         changedColumn     = (init (fst columnPieces)) ++ (token : []) ++ (snd columnPieces) -- ^ insert the token at the end of rhe first List
                                                         gridPieces        = splitAt columnNumber grid
                                                         newGrid           = (init (fst gridPieces)) ++ (changedColumn : []) ++ (snd gridPieces)
                                                     in newGrid
            | otherwise = grid
            
playWith :: [(Int,Char)] -> [[Char]] -> IO b
playWith  round grid = do
          choice <- getChoice
          putStrLn ("your choice: " ++ choice)
          let currentToken = head round
              changedGrid = (dropToken (snd currentToken) (read choice :: Int) grid)
              changedRound = (tail round) ++ (currentToken:[])
          showGrid changedGrid
          playWith changedRound changedGrid
  