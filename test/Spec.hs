-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Main where
    
import Test.Hspec

import Stangry
 
pole1 = Pole 1 1 Wolne Brak
pole2 = Pole 1 2 Wolne Brak
 
main :: IO ()
main = hspec $ do
 
  describe "get303" $ do
    it "should get 3rd elem of tuple" $ do
      get3o3(1,2,3) `shouldBe` 3
      
  describe "replaceRow" $ do
      it "should replace yth elem in list" $ do
          (replace 2 [pole2] [[pole1][pole1][pole1]]) `shouldBe` [[pole1][pole2][pole1]]