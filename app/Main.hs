import System.Environment
import System.Exit
import Data.List
import Lib
import Control.Monad  
import Data.Char 
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

main = do
      hSetBuffering stdout NoBuffering
      args <- getArgs :: IO [String]
      let round = zip [1..] ( nub (head args))
      showGrid 5 5
      putStr "Choose a column: "
      choice <- getLine
      main
