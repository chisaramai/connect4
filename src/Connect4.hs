module Connect4 where

type State    = Bool

data Grid     = Grid 
               [[Char]] -- ^ each position in the grid carries a Character
                         -- ^ '.' for an empty position
                         -- ^ 'X' for a token of the player X
                deriving (Show)

data Connect4 = Connect4 
                [(Int, Char)] -- ^ a list of all the players with their (number, token)
                              -- ^ f.e. (2,'O') for the second player with the token 'O'
                Int           -- ^ number of current player 
                State         -- ^ game state 
                                   -- ^ over    = True
                                   -- ^ running = False
                Grid          -- ^ game grid
                
