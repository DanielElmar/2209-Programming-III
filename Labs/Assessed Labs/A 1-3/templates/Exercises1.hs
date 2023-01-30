{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A1 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

module Exercises (vigenere) where

-- The following two imports are needed for testing, do not delete

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.Char

-- Exercise A1
--20
vigenere :: String -> (String -> String, String -> String)


vigenere "" = (\s -> "", \s -> "")
vigenere k = (  \s -> zipWith (\x y -> chr( 65 + mod (ord x + ord y - 130) 26 )) (filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper s)) [ key!!x | i<-[0..(length s -1)], x<-[i `mod` length key] ] , \es -> zipWith (\x y -> chr( 65 + mod (ord x - ord y - 130) 26 )) (filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper es)) [ key!!x | i<-[0..(length es -1)], x<-[i `mod` length key] ]  )
            where
            key = filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper k)