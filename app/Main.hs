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
      let oldGrid = makeGrid (read (args !! 1) :: Int) (read (args !! 2) :: Int)
      showGrid (oldGrid)
      choice <- getChoice
      putStrLn ("your choice: " ++ choice)
      let newGrid = dropToken 'O' (read choice :: Int) oldGrid
      showGrid (newGrid)
      main
