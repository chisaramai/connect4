module Types
       where

import System.Console.ANSI



type Grid = [String]

data Player     = Player { token   :: Char -- the unique token of this player, chosen when calling the program
                         , color   :: Color -- the color in which the token of this player will be printed during the game
                         } deriving (Show, Eq)

data Game       = Game   { players :: [Player] -- a list of every player in the game
                         , grid    :: Grid -- the current grid
                         , winBy   :: Int -- the neccesary length of a sequence of connected coins/tokens to win the game
                         } deriving (Show, Eq)
