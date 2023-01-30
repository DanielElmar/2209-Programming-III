module Exercises(vigenere) where

import Data.Char

vigenere "" = (\s -> "", \s -> "")
vigenere k = (  \s -> zipWith (\x y -> chr( 65 + mod (ord x + ord y - 130) 26 )) (filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper s)) [ key!!x | i<-[0..(length s -1)], x<-[i `mod` length key] ] , \es -> zipWith (\x y -> chr( 65 + mod (ord x - ord y - 130) 26 )) (filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper es)) [ key!!x | i<-[0..(length es -1)], x<-[i `mod` length key] ]  )
            where
            key = filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper k)