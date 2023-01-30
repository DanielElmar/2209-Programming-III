{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A3 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (amSplit,compress,helperFunc,checkAntiMonotone) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

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




