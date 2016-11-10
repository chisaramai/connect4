module Types
       where

import System.Console.ANSI

type Grid = [String]

data Player     = Player { token   :: Char
                         , color   :: Color
                         } deriving (Show)

data Game       = Game   { players :: [Player]
                         , grid    :: Grid
                         , winBy   :: Int
                         } deriving (Show)
