module Game 
            ( makeGrid
            , dropToken
            ) where

makeGrid m n       =  replicate n (replicate m '.') -- ^ sets up a m * n grid, filled with points at each position
                       

dropToken :: Char -> Int -> [[Char]] -> [[Char]]
dropToken token columnNumber grid = 
                    let columnIndex       = columnNumber - 1 -- ^ f.e. 1234 -> 0123
                        originalColumn    = grid !! columnIndex -- ^ extract the chosen column
                        numberOfEmptyRows = filter (== '.') originalColumn -- ^ scan how many free rows are left in that column
                        columnPieces      = splitAt (length numberOfEmptyRows) originalColumn -- ^ so it will look like this: (["..."],["XO"])
                        changedColumn     = (init (fst columnPieces)) ++ (token : []) ++ (snd columnPieces) -- ^ insert the token at the end of rhe first List
                        gridPieces        = splitAt columnNumber grid
                        newGrid           = (init (fst gridPieces)) ++ (changedColumn : []) ++ (snd gridPieces)
                    in newGrid
