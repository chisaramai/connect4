module Main (main) where

import             Criterion.Main (bench, bgroup, defaultMain, nf)
import             GridScan(scanDiags)

main :: IO()
main = defaultMain
    [ bgroup "GridScan" [ bench "5 times 3 grid win by connected 1"   $ nf scanDiags ["ABC","DEF","GHI","JKL","MNO"]
                        , bench "5 times 5 grid win by connected 4"   $ nf scanDiags ["ABCXC","DEFDS","GHIWD","JKLSD","MSDNO"]
                        , bench "7 times 6 grid win by connected 4"   $ nf scanDiags ["ABCSXC","DEFSDS","GHSIWD","JKLSSD","MSDSNO","HSUSHS","HSJSKA"]
                        ] 
    ]
