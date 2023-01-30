{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (neighbours,findBonding, removeItem, allPossibleMatches, helper ,Position(..), Board(..), label, empty, newboard, moves, slide, adj, replacePosVal, deconstructBoard) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
-- Put ALL of your own import statements here:

import Data.List
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe


-- Exercise A4
type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

neighbours ::  Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours 0 m p ps = []
neighbours k m p [] = []
neighbours k m p ps | k < 0 = error "Taking negative amounts of Points "
                    | otherwise = take (min k (length ps)) (map snd (sortBy (compare `on` fst) (zip (map (m p) ps) ps)))





-- Exercise A5

removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys

allPossibleMatches :: (a -> a -> Bool) -> a -> [a] -> [a]
allPossibleMatches p x xs = filter (p x) xs

helper :: Eq a => (a -> a -> Bool) -> [a] -> [[(a,a)]]
helper p [] = [[]]
helper p (x:xs) = [(x,mx):(mx,x):rest | mx <- allPossibleMatches p x xs, rest <- helper p (removeItem mx xs)]

findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding p xs = (listToMaybe . helper p) xs





-- Exercise A6
-- Do not modify this datatype
data Position = NW | N | NE | W | M | E | SW | S | SE
    deriving (Eq, Ord, Show)

listOfPositions = [NW,N,NE,W,M,E,SW,S,SE]

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

