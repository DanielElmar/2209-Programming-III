{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A2 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (frequency,count) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq



-- Exercise A2
frequency :: Int -> String -> [[(Char,Int)]]

import Data.Char
import Data.Function (on)
import Data.List (sortBy)

count :: String -> [(Char, Int)]
count s = [ (c,length $ filter (== c) (map toUpper s)) | c <- ['A'..'Z'], (length $ filter (== c) (map toUpper s)) /= 0 ]


frequency n ct | n <= length ct =  map (sortBy (flip compare `on` snd)) (map (sortBy (compare `on` fst)) [  count s   | s <-[ [ct!!(offset + (offsetMultiple * n))  | offsetMultiple <- [0..quot(length ct - offset - 1) n ] ] | offset <- [0..minimum[n-1,length ct - 1]]  ]])
               | n > length ct = (map (sortBy (flip compare `on` snd)) (map (sortBy (compare `on` fst)) [  count s   | s <-[ [ct!!(offset + (offsetMultiple * n))  | offsetMultiple <- [0..quot(length ct - offset - 1) n ] ] | offset <- [0..minimum[n-1,length ct - 1]]  ]])) ++ [ [] | i<-[0..(n - length ct -1)] ]