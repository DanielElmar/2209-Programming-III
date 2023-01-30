{-# LANGUAGE DeriveGeneric #-}

--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A6 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

module Exercises (Position(..), Board(..),label, empty, newboard, moves, slide, adj, replacePosVal, deconstructBoard ) where



-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq 
-- Put ALL of your own import statements here:





--
-- Do not modify this datatype
data Position = NW | N | NE | W | M | E | SW | S | SE
    deriving (Eq, Ord, Show)

listOfPositions = [NW,N,NE,W,M,E,SW,S,SE]

-- Exercise A6
-- Provide your own definition of the Board datatype here 
data Board a = B [(Position, Maybe a)] deriving (Eq, Ord, Show)


-- Define all of the board functions below:
newboard :: [a] -> Board a
newboard xs | not $ 8 == length xs = error "You must input a list of length 8"
            | otherwise = B $ zip (listOfPositions) ((map (\x -> Just x ) xs ) ++ [Nothing]) 

label :: Position -> Board a -> Maybe a
label pos (B(x:xs)) | fst x == pos = snd x
                    | otherwise = label pos (B(xs))

empty :: Eq a => Board a -> Position
empty (B []) = error "No empty tile in puzzle...."
empty (B(x:xs)) | snd x == Nothing = fst x
                | otherwise = empty (B(xs))

adj :: Position -> [Position]
adj pos = case pos of NW -> [N,W]
                      N -> [NE,M,NW]
                      NE -> [E,N]
                      W -> [NW,M,SW]
                      M -> [N,E,S,W]
                      E -> [NE,SE,M]
                      SW -> [W,S]
                      S -> [M,SE,SW]
                      SE -> [E,S]

moves :: Eq a => Board a -> [Position]
moves board = adj $ empty board


replacePosVal :: Position -> Maybe a -> [(Position, Maybe a)] -> [(Position, Maybe a)]
replacePosVal _ _ [] = []
replacePosVal pos newVal (x:xs)
   | pos == fst x =  (pos, newVal):xs
   | otherwise =  x:replacePosVal pos newVal xs


deconstructBoard :: Board a -> [(Position, Maybe a)]
deconstructBoard (B (xs)) = xs


slide :: Eq a => Position -> Board a -> Board a
slide pos board | not $ elem pos (moves board) = error "Cant move that tile Sorry."
                | otherwise = B $ replacePosVal pos Nothing $ replacePosVal (empty board) (label pos board) (deconstructBoard board)

