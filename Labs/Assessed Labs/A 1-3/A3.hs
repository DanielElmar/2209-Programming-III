module Exercises(amSplit,compress,helperFunc,checkAntiMonotone ) where
import Data.List

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


