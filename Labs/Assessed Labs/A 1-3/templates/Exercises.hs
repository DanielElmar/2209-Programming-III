{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 1 OF COURSEWORK 1 for COMP2209, 2021
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2021

module Exercises (vigenere,frequency,count,amSplit,compress,helperFunc,checkAntiMonotone) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

import Data.Char
import Data.Function (on)
import Data.List (sortBy)


-- Exercise A1
--20
vigenere :: String -> (String -> String, String -> String)

vigenere "" = (\s -> "", \s -> "")
vigenere k = (  \s -> zipWith (\x y -> chr( 65 + mod (ord x + ord y - 130) 26 )) (filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper s)) [ key!!x | i<-[0..(length s -1)], x<-[i `mod` length key] ] , \es -> zipWith (\x y -> chr( 65 + mod (ord x - ord y - 130) 26 )) (filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper es)) [ key!!x | i<-[0..(length es -1)], x<-[i `mod` length key] ]  )
            where
            key = filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper k)

-- Exercise A2
frequency :: Int -> String -> [[(Char,Int)]]
--30

count :: String -> [(Char, Int)]
count s = [ (c,length $ filter (== c) (map toUpper s)) | c <- ['A'..'Z'], (length $ filter (== c) (map toUpper s)) /= 0 ]

frequency n ct | n <= length ct =  map (sortBy (flip compare `on` snd)) (map (sortBy (compare `on` fst)) [  count s   | s <-[ [ct!!(offset + (offsetMultiple * n))  | offsetMultiple <- [0..quot(length ct - offset - 1) n ] ] | offset <- [0..minimum[n-1,length ct - 1]]  ]])
               | n > length ct = (map (sortBy (flip compare `on` snd)) (map (sortBy (compare `on` fst)) [  count s   | s <-[ [ct!!(offset + (offsetMultiple * n))  | offsetMultiple <- [0..quot(length ct - offset - 1) n ] ] | offset <- [0..minimum[n-1,length ct - 1]]  ]])) ++ [ [] | i<-[0..(n - length ct -1)] ]

-- Exercise A3
amSplit :: Ord a => [a] -> [[a]]

amSplit [] = []
amSplit xs = helperFunc [] xs


compress []     = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)


helperFunc :: Ord a => [a] -> [a] -> [[a]]

helperFunc x [] = x : []
helperFunc accList restOfList@(x:xs) | checkAntiMonotone (compress (accList ++ [x])) == True = helperFunc (accList ++ [x]) xs
                                     | checkAntiMonotone (compress (accList ++ [x])) == False = accList : helperFunc [x] xs

checkAntiMonotone :: Ord a => [a] -> Bool

checkAntiMonotone [] = True
checkAntiMonotone (_:[]) = True
checkAntiMonotone (_:_:[]) = True
checkAntiMonotone (x:y:z:zs) | x >= y && y <= z = checkAntiMonotone (y:z:zs)
                             | x <= y && y >= z = checkAntiMonotone (y:z:zs)
                             | otherwise = False



