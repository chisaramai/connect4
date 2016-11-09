module GridScan
       ( scan
       , scanIt
       , scanCols
       , scanDiags
       , scanRows
       , scanX  
       , diag)
       where

import Data.List
import Data.Set
import qualified Data.Set as Set
  
scan :: [String] -> Int -> Set.Set(Int,Char)
scan grid winSequL = Data.Set.filter ((/= '.') . snd) (unions [scanRows grid,  scanCols grid, scanDiags grid winSequL])

scanRows :: [String] -> Set.Set(Int,Char)
scanRows grid = scanCols (transpose grid)

scanCols :: [String] -> Set.Set(Int,Char)
scanCols = Data.Set.unions . Data.List.map scanIt
{- |
scans all diagonals of the grid
>>> scanDiags ["OOX","OXO","XOO"] 1
fromList [(3,'X'),(1,'O'),(1,'X'),(2,'O'),(2,'X')]
-}
scanDiags :: [String] -> Int -> Set.Set(Int,Char)
scanDiags [] _ = Set.empty
scanDiags grid winSequL
                                       | size == winSequL  = scanX grid -- scan the two largest diagonals
                                       | size >  winSequL  = Set.unions -- scans the hole grid and it's four subgrids
                                                                        [ scanX grid -- scan the two largest diagonals
                                                                        , scanDiags (tail grid) winSequL -- remove first column
                                                                        , scanDiags (init grid) winSequL -- remove last column
                                                                        , scanDiags (init (transpose grid)) winSequL -- remove last row
                                                                        , scanDiags (tail (transpose grid)) winSequL -- remove first row
                                                                        ]  -- this is costly to compute 
                                       | size <  winSequL  = Set.empty
                                     where size = length grid
{- |
scans only the largest diagonals of the grid
>>> scanX ["OOX","OXO","XOO"]
fromList [(3,'X'),(1,'O'),(1,'X')]
-}
scanX :: [String] -> Set.Set (Int, Char)
scanX grid = Data.Set.union (scanIt (diag grid)) (scanIt (diag (reverse grid)))

{- | scans one string, which is a column or a row in this case, for the longest connected sequence of tokens
     and returns this kind of tupel: (length of the sequence, the token with the longest character)
>>> scanIt "..XXXOOOO"
fromList [(2,'.'),(3,'X'),(4,'O')]
-}
scanIt :: String -> Set(Int,Char)
scanIt xs =        let lengths = Data.List.map length (group xs)
                       heads   = Data.List.map   head (group xs)
                   in  Data.Set.fromList (zip lengths heads)

diag :: [String] -> String
diag x = let diagonal = zipWith (!!) x [0..(length (head x) - 1)]
         in  diagonal
