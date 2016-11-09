module Lib
    ( getChoiceOf
    , dropToken
    , playWith
    , findWinner
    , makeGrid
    , showGrid
    , printToken
    , printAllToken
    , concatGrid
    , endGame
    ) where

import GridScan
import Unstable
import System.IO
import System.Environment
import Data.List
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.Map
import System.Console.ANSI
import Control.Concurrent

getChoiceOf :: [String] -> (Char, Color) -> Int -> Int -> IO Int
getChoiceOf grid  player from to = do
            hSetBuffering stdout NoBuffering
            printAllToken [player] ("\nPlayer " ++ [fst player] ++ ", Choose column: ")
            choice <- getLine :: IO String
            if not(null choice)
               && isDigit (head choice)
               && from <= (read choice :: Int)
               && (read choice :: Int) <= to
               && '.' == head (grid !! ((read choice :: Int) - 1)) -- is the chosen column full ?
                then
                 return (read choice :: Int)
                else getChoiceOf grid player from to -- loop back


makeGrid :: Int -> Int -> [String]
makeGrid m n       =  replicate n (replicate m '.') -- sets up a m * n grid, filled with points at each position


dropToken :: Char -> Int -> [String] -> [String]
dropToken token columnNumber grid =
         let columnIndex       = columnNumber - 1 -- f.e. 1234 -> 0123
             originalColumn    = grid !! columnIndex -- extract the chosen column
             emptyRows         = filter (== '.') originalColumn -- scan how many free rows are left in that column
             columnPieces      = splitAt (length emptyRows) originalColumn -- so it will look like this: (["..."],["XO"])
             changedColumn     = init (fst columnPieces) ++ [token] ++ snd columnPieces -- insert the token at the end of the first List
             gridPieces        = splitAt columnNumber grid
             newGrid           = init (fst gridPieces) ++ [changedColumn] ++ snd gridPieces
         in newGrid

            
playWith :: [(Char, Color)] -> [String] -> Int -> IO ()
playWith round grid winSequL = do
                                showGrid grid round -- display current version of the grid
                                let currentPlayer = head round -- get first player in line, this is always the current player
                                let currentToken = fst currentPlayer -- get the character of the current player
                                choice <- getChoiceOf grid currentPlayer 1 (length grid) -- ask the current player in which column he wants to drop a coin
                                let newGrid  = dropToken currentToken choice grid -- dropToken returns the new current version of the grid
                                    changedRound = tail round ++ [currentPlayer] -- the current player becomes the last in line
                                    winner       = findWinner (scan newGrid winSequL) winSequL
                                    gridIsFull     = '.' `notElem` concat newGrid
                                if isNothing winner
                                    then
                                      if gridIsFull
                                      then
                                        do
                                        showGrid newGrid round
                                        printGameOver >> putStr "\n"
                                      else playWith changedRound newGrid winSequL
                                else do
                                      showGrid newGrid round
                                      endGame (fromJust winner) newGrid round
                                 

endGame :: Char -> [String] -> [(Char,Color)] -> IO()
endGame player grid round =  do
                              printGameOver
                              putStr " ... the winner is "
                              setSGR [SetUnderlining SingleUnderline, SetConsoleIntensity BoldIntensity]
                              printAllToken round ("player " ++ [player] ++ "\n")
                              setSGR []
                              
findWinner :: Data.Set.Set(Int,Char) -> Int -> Maybe Char
findWinner xs winSequ = do
                 let maxSequ = foldr (\(a,b) (c,d) -> if a > c then (a,b) else (c,d)) (0,'X') xs 
                 if fst maxSequ < winSequ
                      then Nothing
                      else Just (snd maxSequ)

printGameOver = do
                   setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Green]
                   putStr "Game Over!"
                   setSGR []

{- | concatinates all rows of a grid to a long string
-}
concatGrid :: [String] -> String
concatGrid grid = intercalate "\n" (transpose grid
                            ++ [filter isAlphaNum (unwords (map show [1..(length grid)]))]   -- show column numbers
                            ++ [replicate (length grid) '-']) -- show a separating line

                  
{- | prints each Char of a String to the Console.
     if one of the Char's is in the player's List it is printed colorized
-}                   
printAllToken :: [(Char,Color)] -> String -> IO()
printAllToken players = mapM_ (printToken players)

{- | prints a given Char to the Console.
     if the Char is in the list of players, it is printed in the defined color
-}
printToken :: [(Char,Color)] -> Char -> IO()
printToken players x = do
                       setSGR [SetConsoleIntensity BoldIntensity]
                       if x `elem` map fst players
                                 then do
                                   setSGR [SetColor Foreground Vivid (Data.Map.fromList players Data.Map.! x )]
                                   putChar x -- prints it     colorized
                                   setSGR []
                                 else
                                   putChar x -- prints it non-colorized
                                   
{- | prints the given grid to the Console.
     Characters from the player list are printed in the corresponding color
-}
showGrid :: [String] -> [(Char,Color)] -> IO()
showGrid grid players = do
                    clearScreen
                    setSGR [SetConsoleIntensity BoldIntensity]
                    putStrLn ("\n" ++ replicate (length grid) '-')
                    printAllToken players (intercalate "\n" (transpose grid))
                    setSGR [SetConsoleIntensity BoldIntensity]
                    putStrLn ("\n" ++ filter isAlphaNum (unwords (map show [1..(length grid)])))   -- show column numbers
                    setSGR []
                    
