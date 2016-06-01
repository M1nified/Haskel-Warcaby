-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Main where
    
import Test.Hspec

import GameTest
import StangryTest
 
main :: IO ()
main = hspec $ do
  StangryTest.test
  GameTest.test