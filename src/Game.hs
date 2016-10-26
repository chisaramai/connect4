module Game where

setupGrid m n       =  replicate n (replicate m '.')

dropToken :: Char -> Int -> [[Char]] -> [[Char]]
dropToken token columnNumber grid = 
                    let originalColumn    = grid !! columnNumber
                        numberOfEmptyRows = filter (== '.') originalColumn
                        columnPieces      = splitAt (length numberOfEmptyRows) originalColumn 
                        changedColumn     = (init (fst columnPieces)) ++ (token : []) ++ (snd columnPieces)
                        gridPieces        = splitAt columnNumber grid
                        newGrid           = (init (fst gridPieces)) ++ (changedColumn : []) ++ (snd gridPieces)
                    in newGrid
