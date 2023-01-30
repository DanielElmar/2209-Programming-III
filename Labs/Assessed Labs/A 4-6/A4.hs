--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A4 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (neighbours) where

import Data.List
import Data.Function (on)
import Data.List (sortBy)

type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

-- Exercise A4

neighbours :: Int -> Metric a -> Point a -> [Point a] -> [Point a]

neighbours 0 m p ps = []
neighbours k m p [] = []
neighbours k m p ps | k < 0 = error "Taking negative amounts of Points "
                    | otherwise = take (min k (length ps)) (map snd (sortBy (compare `on` fst) (zip (map (m p) ps) ps)))


