{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec (spec) where

import           Lib                    (findWinner)
import           GridScan               (scanIt
                                        ,scanCols
                                        ,scanRows
                                        ,scanDiags
                                        ,scan)
import           Test.Hspec
import           Test.QuickCheck
import           Data.Set (Set)
import           qualified Data.Set as Set


spec :: Spec
spec = do
  
    describe "scanIt" $ do
        it "scanIt \"....XXOOOOO\"" $
            scanIt "....XXOOOOO" `shouldBe` Set.fromList [(4,'.'),(2,'X'),(5,'O')]
        it "scanIt \"....XXXOO\"" $
            scanIt "....XXXOO" `shouldBe` Set.fromList [(4,'.'),(3,'X'),(2,'O')]

    describe "scanCols" $ do
        it "scanCols [\"XO.\",\".O.\",\"OO.\"]" $
            scanCols ["XO.",".O.","OO."] `shouldBe` Set.fromList [(1,'X'),(1,'O'),(1,'.'),(1,'.'),(1,'O'),(1,'.'),(2,'O'),(1,'.')]
        it "scanCols [\"...\",\"...\",\"...\"]" $
            scanCols ["...","...","..."] `shouldBe` Set.fromList [(3,'.'),(3,'.'),(3,'.')]
                
    describe "scanDiags" $ do
        it "scanDiags [\"XOO\",\".XO\",\"OOX\"] 1" $
            scanDiags ["XOO",".XO","OOX"] 1 `shouldBe` Set.fromList [(3,'X'),(2,'O'),(2,'X'),(1,'.'),(1,'O'),(1,'X')]       
        it "scanDiags [\"ABC\",\"DEF\",\"GHI\"] 1" $
            scanDiags ["ABC","DEF","GHI"] 1 `shouldBe` Set.fromList [(1,'A'),(1,'B'),(1,'C'),(1,'D'),(1,'E'),(1,'F'),(1,'G'),(1,'H'),(1,'I')]
        it "scanDiags [\"ABC\",\"DEF\",\"GHI\",\"JKL\",\"MNO\"] 1" $
            scanDiags ["ABC","DEF","GHI","JKL","MNO"] 1 `shouldBe` Set.fromList [(1,'A'),(1,'B'),(1,'C'),(1,'D'),(1,'E'),(1,'F'),(1,'G'),(1,'H'),(1,'I'),(1,'J'),(1,'K'),(1,'L'),(1,'M'),(1,'N'),(1,'O')]
        it "scanDiags [\"XOX\",\"OXO\",\"XOX\"] 1" $
           scanDiags ["XOX","OXO","XOX"] 1 `shouldBe` Set.fromList [(3,'X'),(2,'O'),(2,'X'),(1,'O'),(1,'X')]
        it "scanDiags [\"XOXX\",\"OXOO\",\"XOXO\"] 1" $
           scanDiags ["XOXX","OXOO","XOXO"] 1 `shouldBe` Set.fromList [(3,'X'),(3,'O'),(2,'O'),(2,'X'),(1,'O'),(1,'X')]
    
    describe "scanRows" $ do
        it "scanRows [\"XO.\",\".O.\",\"OO.\"]" $
            scanRows ["XO.",".O.","OO."] `shouldBe` Set.fromList [(1,'X'),(1,'.'),(1,'O'),(3,'O'),(3,'.')]        
        it "scanRows [\"ABC\",\"DEF\",\"GHI\"]" $
            scanRows ["ABC","DEF","GHI"] `shouldBe` Set.fromList [(1,'A'),(1,'D'),(1,'G'),(1,'B'),(1,'E'),(1,'H'),(1,'C'),(1,'F'),(1,'I')]
            
    describe "scan" $
        it "scan [\"...\",\"...\",\"...\"]" $
            scan ["...","...","..."] 1 `shouldBe` Set.empty

    describe "findWinner" $ do
        it "findWinner [(1,'X'),(1,'.'),(1,'O'),(3,'O'),(3,'.')] 4 " $
            findWinner (Set.fromList [(1,'X'),(1,'.'),(1,'O'),(3,'O'),(3,'.')]) 4 `shouldBe` Nothing
        it "findWinner [] 4 "                                        $
            findWinner Set.empty  4                                      `shouldBe` Nothing


    describe "findWinner . scan" $ do
        it "findWinner (scan [\"..F\",\".BB\",\".FB\"] 4)" $
            findWinner (scan ["..F",".BB",".FB"] 4) 4 `shouldBe` Nothing
        it "findWinner (scan [\"...\",\"...\",\"...\"] 4)" $
            findWinner (scan ["...","...","..."] 4) 4 `shouldBe` Nothing

