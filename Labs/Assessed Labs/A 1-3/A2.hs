module Exercises(frequency,count) where

import Data.Char
import Data.Function (on)
import Data.List (sortBy)

count :: String -> [(Char, Int)]
count s = [ (c,length $ filter (== c) (map toUpper s)) | c <- ['A'..'Z'], (length $ filter (== c) (map toUpper s)) /= 0 ]

{-
frequency :: Int -> String -> [[(Char, Int)]]
frequency n "" = [ [] | i<-[0..n-1] ] 
frequency n ct = map (sortBy (flip compare `on` snd)) (map (sortBy (compare `on` fst)) [  count s   | s <-[ [ct!!(offset + (offsetMultiple * n))  |offsetMultiple <- [0..quot(length ct - offset - 1) n ] ] | offset <- [0..minimum[n-1,length ct - 1]]  ]])

-}


frequency :: Int -> String -> [[(Char, Int)]]
--frequency n "" = [ [] | i<-[0..n-1] ] 

frequency n ct | n <= length ct =  map (sortBy (flip compare `on` snd)) (map (sortBy (compare `on` fst)) [  count s   | s <-[ [ct!!(offset + (offsetMultiple * n))  | offsetMultiple <- [0..quot(length ct - offset - 1) n ] ] | offset <- [0..minimum[n-1,length ct - 1]]  ]])
               | n > length ct = (map (sortBy (flip compare `on` snd)) (map (sortBy (compare `on` fst)) [  count s   | s <-[ [ct!!(offset + (offsetMultiple * n))  | offsetMultiple <- [0..quot(length ct - offset - 1) n ] ] | offset <- [0..minimum[n-1,length ct - 1]]  ]])) ++ [ [] | i<-[0..(n - length ct -1)] ]