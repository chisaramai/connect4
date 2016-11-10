import System.Environment
import System.Exit
import Data.List
import Lib
import Types
import Control.Monad  
import Data.Char
import System.Random
import System.Console.ANSI
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)


main = do
          hSetBuffering stdout NoBuffering
          args <- getArgs :: IO [String]
          let grid     = mkGrid (read (args !! 1) :: Int) (read (args !! 2) :: Int) -- parse row & clumn for grid setup
              winSequL = read (args !! 3) :: Int -- define by which length of a sequence the game ends and someone wins
          themeNr <- rollDice :: IO Int -- pick a random theme
          let players  = zipWith Player (nub (head args)) (theme themeNr)  -- define players
          playWith (Game players grid winSequL) -- start game
          
