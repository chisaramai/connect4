import System.Environment
import System.Exit
import Data.List
import Lib
import Control.Monad  
import Data.Char 
import System.Console.ANSI
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)


main = do
          hSetBuffering stdout NoBuffering
          args <- getArgs :: IO [String]
          let round = zip (nub (head args)) [Yellow, Cyan, Blue, Green, Magenta, Red]
          let grid = makeGrid (read (args !! 1) :: Int) (read (args !! 2) :: Int)
          let winSequL = read (args !! 3) :: Int
          playWith round grid winSequL
          
