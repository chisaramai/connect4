module Lib
    ( theme
    , getChoiceOf
    , dropToken
    , playWith
    , findWinner
    , mkGrid
    , showGrid
    , printToken
    , printAllToken
    , concatGrid
    , endGame
    , rollDice
    ) where

import GridScan
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
import Types
import System.Random




{- | 720 different themes
   | you can chose one from 0 up to 719
-}
theme :: Int -> [Color]
theme x = themes !! x
             where colors = [ Yellow, Cyan, Blue, Green, Magenta, Red ]
                   themes =  nub (permutations colors)



{- | interaction with the user/player of this game
   | he needs to enter a number beween 1 and n
-}
getChoiceOf :: Grid -> Player -> Int -> Int -> IO Int
getChoiceOf    grid    player    from   to  =
  
                                    do
                                    hSetBuffering stdout NoBuffering
                                    printAllToken [player] ("\nPlayer " ++ [token player] ++ ", Choose column: ")
                                    choice <- getLine :: IO String
                                    if not(null choice)
                                       && isDigit (head choice)
                                       && from <= (read choice :: Int)
                                       && (read choice :: Int) <= to
                                       && '.' == head (grid !! ((read choice :: Int) - 1)) -- is the chosen column full ?
                                    then
                                       return (read choice :: Int)
                                    else getChoiceOf grid player from to -- loop back


{- |
-}
mkGrid :: Int -> Int -> Grid
mkGrid    m      n                          =  replicate n (replicate m '.') -- sets up a m * n grid, filled with points at each position


{- | dropToken
   |    takes a
-}
dropToken :: Char -> Int       -> Grid -> Grid
dropToken    token   choice grid =
  
                    let columnIndex       = choice - 1 -- f.e. 1234 -> 0123
                        originalColumn    = grid !! columnIndex -- extract the chosen column
                        emptyRows         = filter (== '.') originalColumn -- scan how many free rows are left in that column
                        columnPieces      = splitAt (length emptyRows) originalColumn -- so it will look like this: (["..."],["XO"])
                        changedColumn     = init (fst columnPieces) ++ [token] ++ snd columnPieces -- insert the token at the end of the first list
                        gridPieces        = splitAt choice grid
                        newGrid           = init (fst gridPieces) ++ [changedColumn] ++ snd gridPieces -- form the new grid
                    in newGrid



{- | playWith is the main loop of the game
   | when this loop ends the game ends
   | takes a Game and creates a new Game in each loop while the game continues
   | the game stops
   |    if all positions in the grid are occupied
   |    or
   |    after calling the dropToken function, a player has connected a sequence long enough to win the game
-}            
playWith :: Game -> IO ()
playWith    game                  =

                do
                showGrid game -- display current version of the grid
                let currentPlayer = head (players game) -- get first player in line, this is always the current player
                let currentToken  = token currentPlayer -- get the token of the current player
                choice <- getChoiceOf (grid game) currentPlayer 1 (length (grid game)) -- ask the current player in which column he wants to drop a coin
                let newGrid       = dropToken currentToken choice ( grid game ) -- dropToken returns the new current version of the grid
                    newPlayers    = tail ( players game ) ++ [currentPlayer] -- the current player becomes the last in line
                    winner        = findWinner (scan newGrid ( winBy game ) ) ( winBy game )
                    newGame       = Game newPlayers newGrid ( winBy game )
                    gridIsFull    = '.' `notElem` concat newGrid
                if isNothing winner
                then
                   if gridIsFull
                   then
                       do
                       showGrid newGame
                       printGameOver >> putStr "\n"
                   else playWith newGame
                else do
                     showGrid newGame
                     endGame (fromJust winner) newGame


                                 
{- | displays "Game over! and the winner of the game
-}
endGame :: Char -> Game -> IO()
endGame    token   game  =

                              do
                              printGameOver
                              putStr " ... winner is "
                              setSGR [SetUnderlining SingleUnderline, SetConsoleIntensity BoldIntensity]
                              printAllToken (players game) ("player " ++ [token] ++ "\n")
                              setSGR []

                              
                              
{- | takes a set of token sequences and determines who has won the game
   | by     searching for the longest sequence (connected tokens)
   |    and prooving if it is long enough for winning the game
-}
findWinner :: Set.Set(Int,Char) -> Int -> Maybe Char
findWinner    xs                   winSequ =
  
                 do
                 let maxSequ = foldl (\(a,b) (c,d) -> if a > c then (a,b) else (c,d)) (0,'X') xs 
                 if fst maxSequ < winSequ
                      then Nothing
                      else Just (snd maxSequ)
                           


{- | prints "Game over!" in color
-}
printGameOver =    do
                   setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
                   putStr "Game Over!"
                   setSGR []

                   

{- | concatinates all rows of a grid to a long string
-}
concatGrid :: Grid -> String
concatGrid    grid = intercalate "\n" (transpose grid
                            ++ [filter isAlphaNum (unwords (map show [1..(length grid)]))]   -- show column numbers
                            ++ [replicate (length grid) '-']) -- show a separating line


                     
{- | prints each Char of a String to the Console.
   | if one of the Chars is in the player's List it is printed colorized
-}                   
printAllToken :: [Player] -> String -> IO()
printAllToken players = mapM_ (printToken players)



{- | prints a given Char to the Console.
   | if the Char is in the list of players, it is printed in the defined color
-}
printToken :: [Player] -> Char -> IO()
printToken players x = do
                       setSGR [SetConsoleIntensity BoldIntensity]
                       if x `elem` map token players
                                 then do
                                   setSGR [SetColor Foreground Vivid (color (head (filter ( (== x) . token ) players)))]
                                   putChar x -- prints it     colorized
                                   setSGR []
                                 else
                                   putChar x -- prints it non-colorized


                                   
{- | prints the given grid to the Console.
     Characters from the player list are printed in the corresponding color
-}
showGrid :: Game -> IO()
showGrid game = do
                    clearScreen
                    setSGR [SetConsoleIntensity BoldIntensity]
                    putStrLn ("\n" ++ replicate (length (grid game)) '-')
                    printAllToken (players game) (intercalate "\n" (transpose (grid game)))
                    setSGR [SetConsoleIntensity BoldIntensity]
                    putStrLn ("\n" ++ filter isAlphaNum (unwords (map show [1..(length (grid game))])))   -- show column numbers
                    setSGR []


                    
{- | produces random numbers from 1 to 6!
-}
rollDice :: IO Int
rollDice = getStdRandom (randomR (1, max))
              where max = length $ permutations [ Yellow, Cyan, Blue, Green, Magenta, Red ]
