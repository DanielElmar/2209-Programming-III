--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A5 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (findBonding, removeItem, allPossibleMatches, helper) where

import Data.List
import Data.Maybe

-- Exercise A5

findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe[(a,a)]

removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys

allPossibleMatches :: (a -> a -> Bool) -> a -> [a] -> [a]
allPossibleMatches p x xs = filter (p x) xs

helper :: Eq a => (a -> a -> Bool) -> [a] -> [[(a,a)]]
helper p [] = [[]]
helper p (x:xs) = [(x,mx):(mx,x):rest | mx <- allPossibleMatches p x xs, rest <- helper p (removeItem mx xs)]

-- if empty list listToMaybe turns it to a Nothing type or to a Just if not empty
findBonding p xs = (listToMaybe . helper p) xs




