import System.Environment
import System.Exit
import Data.List
import Lib
import Game
import Control.Monad  
import Data.Char 
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)


main = do
          hSetBuffering stdout NoBuffering
          args <- getArgs :: IO [String]
          let round = zip [1..] (nub (args !! 0))
          let grid = makeGrid (read (args !! 1) :: Int) (read (args !! 2) :: Int)
          playWith round grid
