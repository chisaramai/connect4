module Lib
    ( showGrid 
    ) where

import System.IO
import System.Environment
import Data.List

--showGrid column row :: IO()
showGrid column row = do
      putStrLn ( intercalate "\n" (["\n"] ++ ["-----"] ++ ( replicate row (replicate column '.')) ++ ["01234"]))
