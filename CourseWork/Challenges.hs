{-# LANGUAGE DeriveGeneric #-}
-- Author: Daniel Elmar
-- ID: 31820638
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),LetExpr(..),CLExpr(..),
                   calcBBInteractions,
                   solveBB,
                   prettyPrint,
                   parseLet,
                   clTransform,
                   innerRedn1,outerRedn1,innerCLRedn1,outerCLRedn1,compareInnerOuter
                   )
where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

import Data.List
import Data.Function


instance NFData CLExpr
instance NFData LetExpr
instance NFData LamExpr
instance NFData Marking
instance NFData Side


-- Challenge 1
-- Calculate Interactions in the Black Box

type Atoms = [ Pos ]
type Interactions = [  ( EdgePos , Marking )  ]
type Pos = (Int, Int)   -- top left is (1,1) , bottom right is (N,N) where N is size of grid    (COL, ROW)
type EdgePos = ( Side , Int ) -- int range is 1 to N where N is size of grid

data Side = North | East | South | West
            deriving (Show, Eq, Ord)

data Marking =  Absorb | Reflect | Path EdgePos
                deriving (Show, Eq)

type Traveling = (Pos, Side) -- a position and a direction of traveling

-- converts EdgePos type to a Traveling type with defualt values OUTSIDE the board
edgeToTravaling :: EdgePos -> Int -> Traveling
edgeToTravaling (side,num) size  | side == North = ((num,0), South) -- atoms
                      | side == East = ((size + 1,num), West)
                      | side == South = ((num , size + 1), North)
                      | side == West = ((0,num), East)

-- converts EdgePos type to a Pos type with defualt values OUTSIDE the board
edgeToPos :: EdgePos -> Int -> Pos
edgeToPos (side, num) size = case side of
                              North -> (num,0)
                              East -> (size+1,num)
                              South -> (num,size+1)
                              West -> (0,num)
                              otherwise -> error "Not Side"

stepTravel :: Traveling -> Traveling -- Pos
stepTravel ((c,r), dir)   | dir == North = ((c,r-1), North)
                          | dir == East = ((c+1,r), East)
                          | dir == South = ((c,r+1), South)
                          | dir == West = ((c-1,r), West)

getPosAheadLeft :: Traveling -> Pos
getPosAheadLeft ((c,r), dir)  | dir == North = (c-1,r-1)
                              | dir == East = (c+1,r-1)
                              | dir == South = (c+1,r+1)
                              | dir == West = (c-1,r+1)

getPosAheadCentre :: Traveling -> Pos
getPosAheadCentre ((c,r), dir)  | dir == North = (c,r-1)
                                | dir == East = (c+1,r)
                                | dir == South = (c,r+1)
                                | dir == West = (c-1,r)

getPosAheadRight :: Traveling -> Pos
getPosAheadRight ((c,r), dir)  | dir == North = (c+1,r-1)
                                | dir == East = (c+1,r+1)
                                | dir == South = (c-1,r+1)
                                | dir == West = (c-1,r-1)

atomAheadLeft :: Traveling -> [Pos] -> Bool
atomAheadLeft t@((c,r), dir) atoms = elem (getPosAheadLeft t) atoms

atomAheadCentre :: Traveling -> [Pos] -> Bool
atomAheadCentre t@((c,r), dir) atoms = elem (getPosAheadCentre t) atoms

atomAheadRight :: Traveling -> [Pos] -> Bool
atomAheadRight t@((c,r), dir) atoms = elem (getPosAheadRight t) atoms

rotateDirCW :: Side -> Side
rotateDirCW direction | direction == North = East
                      | direction == East = South
                      | direction == South = West
                      | direction == West = North

rotateDirACW :: Side -> Side
rotateDirACW = rotateDirCW . rotateDirCW . rotateDirCW

-- converts a Pos to a Path Marking
posToPathMarking :: Pos -> Int -> Marking
posToPathMarking (c,r) size = let s = size + 1 in case c of
                            0 -> (Path (West, r))
                            otherwise -> if c == s then (Path (East, r)) else case r of
                                              0 -> (Path (North, c))
                                              otherwise -> if r == s then (Path (South, c)) else error "Not Path"


-- comutes a step forward with in the grid, considers and computes any interactions with
-- Atoms, calls itself to produce the final disination of the full path traveled my the ray
-- aac = atom ahead center
-- aal / r = atom ahead left / right
-- returning the Traveling ((-2,-2), direction) indecates an Absorbsion
itteratePath :: Traveling -> Atoms -> Int -> Bool -> Traveling
itteratePath travel@((c,r), direction) atoms size outOfBondsRulesActive | outOfBondsRulesActive && (c <= 0 || c >= size + 1 || r <= 0 || r >= size + 1) = ((c,r), direction)
                                                                        | (not outOfBondsRulesActive) && aac = ((-2,-2), direction)
                                                                        | (not outOfBondsRulesActive) && (aal || aar)  = travel
                                                                        | aac = ((-2,-2), direction)
                                                                        | aal = itteratePath ((c,r), rotateDirCW direction) atoms size True
                                                                        | aar = itteratePath ((c,r), rotateDirACW direction) atoms size True
                                                                        | otherwise = itteratePath (stepTravel travel) atoms size True
                                                                          where aac = atomAheadCentre travel atoms
                                                                                aal = atomAheadLeft travel atoms
                                                                                aar = atomAheadRight travel atoms

-- Takes an entry point, the board and atoms, and returns the "exit" Marking
tracePath :: EdgePos -> Int -> Atoms -> Marking
tracePath startingEdgePos@(side,num) size atoms  | (num < 1) || (num > size) = error "Starting position invalid"
                                                 | otherwise = let pos@(c1,r1) = fst $ itteratePath (edgeToTravaling startingEdgePos size) atoms size False in
                                                 let startingPos@(c2,r2) = edgeToPos startingEdgePos size in
                                                  if pos == startingPos then Reflect else
                                                    case (c1,r1) of
                                                      (-2,-2) -> Absorb
                                                      otherwise -> posToPathMarking pos size

-- iterates over all valid EdgePos for a given sized board and returns the computed Markings
calcBBInteractions :: Int -> Atoms -> Interactions -- Size of the NxN grid and a list of atoms
calcBBInteractions size atoms = [ ((side,num), tracePath (side,num) size atoms) | side <- [North, East, South, West ], num <- [1..size] ]

-- Challenge 2
-- Solve a Black Box

-- types used to help reprsent the board throught computation
-- the Grid type is a grid with weighted positions
type GridNum = (Pos, Int)
type Grid = [GridNum]


-- check is a Interaction is a Absorb, Reflect, or passes straight through
isApplicableHuristic :: (EdgePos, Marking) -> Bool
isApplicableHuristic (_, Absorb) = True
isApplicableHuristic (_, Reflect) = True
isApplicableHuristic ((side1, num1), Path (side2,num2)) = (side1 /= side2) && (not (oppositeSides side1 side2))


generateEmptyGrid :: Int -> Grid
generateEmptyGrid size = [ ((c,r),0) | c <- [1..size], r <- [1..size]]

-- adjusts the value in a given grid at a given position by a given amount
adjust :: Pos -> Int -> Grid -> Grid
adjust pos value grid = Prelude.map (\ x -> if (fst x) == pos then (pos, ( maybeToNorm(lookup pos grid) + value) ) else x) grid

-- takes a list of positions and adjusts the value of those positions in a given grid by a given amout
adjustGridFromList :: [Pos] -> Int -> Grid -> Grid
adjustGridFromList [] adjustValue grid = grid
adjustGridFromList (pos:poss) adjustValue grid = adjust pos adjustValue (adjustGridFromList poss adjustValue grid)

-- takes a Traveling and them adjusts the values of the entire row or column in the given grid based on the value of the Traveling
-- the values are adjusted a given amout applied to a given function,

-- for exsample with a valAdjust of (\x -> x - 1) would cause the values to be adjust by a decreasing amount, with each position
-- being adjusted by a amount 1 less than the previous position
adjustGridLine :: Traveling -> Int -> Int -> (Int -> Int) -> Grid -> Grid
adjustGridLine travel@((c,r), _) value size valAdjust grid  | c < 1 || c > size = grid
                                                            | r < 1 || r > size = grid
                                                            | otherwise = adjust (c,r) value $ (adjustGridLine (stepTravel travel) (valAdjust value) size valAdjust grid )

oppositeSides :: Side -> Side -> Bool
oppositeSides side1 side2 | (side1 == North) && (side2 == South) = True
                          | (side1 == East) && (side2 == West) = True
                          | (side1 == South) && (side2 == North) = True
                          | (side1 == West) && (side2 == East) = True
                          | otherwise = False

-- converts EdgePos type to a Traveling type with defualt values INSIDE the board
edgeToTravaling1 :: EdgePos -> Int -> Traveling
edgeToTravaling1 (side,num) size  | side == North = ((num,1), South)
                                  | side == East = ((size ,num), West)
                                  | side == South = ((num , size), North)
                                  | side == West = ((1,num), East)

-- converts EdgePos type to a Pos type with defualt values INSIDE the board
edgeToPos1 :: EdgePos -> Int -> Pos
edgeToPos1 (side, num) size = case side of
                              North -> (num,1)
                              East -> (size,num)
                              South -> (num,size)
                              West -> (1,num)
                              otherwise -> error "Not Side"

-- used to to adjust a spesific position in the grid when there exists a path that has taken a 90 degree turn
-- this spesific position may be outside the board and thus this is checked before
adjustDiagnal :: EdgePos -> EdgePos -> Int -> Grid -> Grid
adjustDiagnal (North,num1) (East,num2) size grid | ((num1 - 1) >= 1) && ((num2 + 1) <= size) = adjust ((num1 - 1),(num2 + 1)) 5 grid
adjustDiagnal (North,num1) (West,num2) size grid | ((num1 + 1) <= size) && ((num2 + 1) <= size) = adjust ((num1 + 1),(num2 + 1)) 5 grid

adjustDiagnal (South,num1) (East,num2) size grid | ((num1 - 1) >= 1) && ((num2 - 1) >= 1) = adjust ((num1 - 1),(num2 - 1)) 5 grid
adjustDiagnal (South,num1) (West,num2) size grid | ((num1 + 1) <= size) && ((num2 - 1)>= 1) = adjust ((num1 + 1),(num2 - 1)) 5 grid

adjustDiagnal (East,num1) (South,num2) size grid | ((num1 - 1) >= 1) && ((num2 - 1) >= 1) = adjust ((num2 - 1),(num1 - 1)) 5 grid
adjustDiagnal (East,num1) (North,num2) size grid | ((num1 + 1) <= size) && ((num2 - 1) >= 1) = adjust ((num2 - 1),(num1 + 1)) 5 grid

adjustDiagnal (West,num1) (South,num2) size grid | ((num1 - 1) >= 1) && ((num2 + 1) <= size) = adjust ((num2 + 1),(num1 - 1)) 5 grid
adjustDiagnal (West,num1) (North,num2) size grid | ((num1 + 1) <= size) && ((num2 + 1) <= size) = adjust ((num2 + 1),(num1 + 1)) 5 grid

adjustDiagnal _ _ _ grid =  grid

-- used to get a list of positions with a board that that starts from the West side up untill a given column
-- this lists included positions traced by a ray aswell as adjacent positions
getLeftToRightPoss :: EdgePos -> EdgePos -> Int -> [Pos]
getLeftToRightPoss (West, num1) (_,num2) size | lessBounds && moreBounds = [ (c,r) | r <- [(num1 -1)..(num1 +1)], c <- [1..num2]]
                                              | lessBounds = [ (c,r) | r <- [(num1 -1)..(num1)], c <- [1..num2]]
                                              | moreBounds = [ (c,r) | r <- [(num1)..(num1 +1)], c <- [1..num2]]
                                                where lessBounds = ((num1 - 1) > 0)
                                                      moreBounds = ((num1 + 1) < (size + 1))

-- used to get a list of positions with a board that that starts from the East side up untill a given column
-- this lists included positions traced by a ray aswell as adjacent positions
getRightToLeftPoss :: EdgePos -> EdgePos -> Int -> [Pos]
getRightToLeftPoss (East, num1) (_,num2) size | lessBounds && moreBounds = [ (c,r) | r <- [(num1 -1)..(num1 +1)], c <- [num2..size]]
                                              | lessBounds = [ (c,r) | r <- [(num1 -1)..(num1)], c <- [num2..size]]
                                              | moreBounds = [ (c,r) | r <- [(num1)..(num1 +1)], c <- [num2..size]]
                                                where lessBounds = ((num1 - 1) > 0)
                                                      moreBounds = ((num1 + 1) < (size + 1))

-- used to get a list of positions with a board that that starts from the North side down untill a given row
-- this lists included positions traced by a ray aswell as adjacent positions
getTopToBottomPoss :: EdgePos -> EdgePos -> Int -> [Pos]
getTopToBottomPoss (North, num1) (_,num2) size  | lessBounds && moreBounds = [ (c,r) | c <- [(num1 -1)..(num1 +1)], r <- [1..num2]]
                                                | lessBounds = [ (c,r) | c <- [(num1 -1)..(num1)], r <- [1..num2]]
                                                | moreBounds = [ (c,r) | c <- [(num1)..(num1 +1)], r <- [1..num2]]
                                                  where lessBounds = ((num1 - 1) > 0)
                                                        moreBounds = ((num1 + 1) < (size + 1))

-- used to get a list of positions with a board that that starts from the South side up untill a given row
-- this lists included positions traced by a ray aswell as adjacent positions
getBottomToTopPoss :: EdgePos -> EdgePos -> Int -> [Pos]
getBottomToTopPoss (South, num1) (_,num2) size  | lessBounds && moreBounds = [ (c,r) | c <- [(num1 -1)..(num1 +1)], r <- [num2..size]]
                                                | lessBounds = [ (c,r) | c <- [(num1 -1)..(num1)], r <- [num2..size]]
                                                | moreBounds = [ (c,r) | c <- [(num1)..(num1 +1)], r <- [num2..size]]
                                                  where lessBounds = ((num1 - 1) > 0)
                                                        moreBounds = ((num1 + 1) < (size + 1))

-- takes a list of interations and generates a board with weighted positions based on the rules applied to the each interactons
generateNumberedGrid :: Int -> Interactions -> Grid -> Grid

generateNumberedGrid size [] grid = grid

-- Check Absorbsion case
generateNumberedGrid size ((edgePos, Absorb): inters) grid = generateNumberedGrid size inters (adjustGridLine (edgeToTravaling1 edgePos size) size size (\x -> x-1) grid)

-- Check Reflect Through case
generateNumberedGrid size ((edgePos@(side1,num1), Reflect): inters) grid  | moreBounds && lessBounds = generateNumberedGrid size inters (straightAdjust $ lessAdjust $ moreAdjust grid)  --adjustGridLine (edgeToTravaling edgePos) 5 size (\x -> x) (adjustGridLine ( (edgeToTravaling (side, (num1 + 1))  )) size size (\x -> x-1) (adjustGridLine ( (edgeToTravaling (side, (num1 - 1))  )) size size (\x -> x-1) grid))
                                                                          | lessBounds = generateNumberedGrid size inters (straightAdjust $ lessAdjust grid)
                                                                          | moreBounds = generateNumberedGrid size inters (straightAdjust $ moreAdjust grid)
                                                                          | otherwise = generateNumberedGrid size inters (straightAdjust grid)
                                                                            where
                                                                              lessBounds = ((num1 - 1) > 0)
                                                                              moreBounds = ((num1 + 1) < (size + 1))
                                                                              straightAdjust = adjustGridLine (edgeToTravaling1 edgePos size) (-5) size (\x -> x)
                                                                              lessAdjust = adjustGridLine ( (edgeToTravaling1 (side1, (num1 - 1)) size )) size size (\x -> x-1)
                                                                              moreAdjust = adjustGridLine ( (edgeToTravaling1 (side1, (num1 + 1)) size )) size size (\x -> x-1)

-- Check 90 degree turn case
generateNumberedGrid size ((edgePos@(West,num1), Path (North,num2)):inters) grid = generateNumberedGrid size inters (adjustDiagnal edgePos (North,num2) size (adjustGridFromList (nub ((getLeftToRightPoss edgePos (North, num2) size) ++ (getTopToBottomPoss (North, num2) edgePos size))) (-1) grid))
generateNumberedGrid size ((edgePos@(West,num1), Path (South,num2)):inters) grid = generateNumberedGrid size inters (adjustDiagnal edgePos (South,num2) size (adjustGridFromList (nub ((getLeftToRightPoss edgePos (North, num2) size) ++ (getBottomToTopPoss (South, num2) edgePos size))) (-1) grid))

generateNumberedGrid size ((edgePos@(East,num1), Path (North,num2)):inters) grid = generateNumberedGrid size inters (adjustDiagnal edgePos (North,num2) size (adjustGridFromList (nub ((getRightToLeftPoss edgePos (North, num2) size) ++ (getTopToBottomPoss (North, num2) edgePos size))) (-1) grid))
generateNumberedGrid size ((edgePos@(East,num1), Path (South,num2)):inters) grid = generateNumberedGrid size inters (adjustDiagnal edgePos (South,num2) size (adjustGridFromList (nub ((getRightToLeftPoss edgePos (North, num2) size) ++ (getBottomToTopPoss (South, num2) edgePos size))) (-1) grid))

generateNumberedGrid size ((edgePos@(North,num1), Path (East,num2)):inters) grid = generateNumberedGrid size inters (adjustDiagnal edgePos (East,num2) size (adjustGridFromList (nub ((getTopToBottomPoss edgePos (East, num2) size) ++ (getRightToLeftPoss (East, num2) edgePos size))) (-1) grid))
generateNumberedGrid size ((edgePos@(North,num1), Path (West,num2)):inters) grid = generateNumberedGrid size inters (adjustDiagnal edgePos (West,num2) size (adjustGridFromList (nub ((getTopToBottomPoss edgePos (East, num2) size) ++ (getLeftToRightPoss (West, num2) edgePos size))) (-1) grid))

generateNumberedGrid size ((edgePos@(South,num1), Path (East,num2)):inters) grid = generateNumberedGrid size inters (adjustDiagnal edgePos (East,num2) size (adjustGridFromList (nub ((getBottomToTopPoss edgePos (West, num2) size) ++ (getRightToLeftPoss (East, num2) edgePos size))) (-1) grid))
generateNumberedGrid size ((edgePos@(South,num1), Path (West,num2)):inters) grid = generateNumberedGrid size inters (adjustDiagnal edgePos (West,num2) size (adjustGridFromList (nub ((getBottomToTopPoss edgePos (West, num2) size) ++ (getLeftToRightPoss (West, num2) edgePos size))) (-1) grid))

-- error Case
generateNumberedGrid _ (inter:inters) _ = error ("No Patteren" ++ show(inter))

-- tests is a list of atoms produces the same list of interactions as the given list of interactions
testAtoms ::  Int -> Interactions -> [[Pos]] -> [Pos]
testAtoms _ _ [] = []
testAtoms size interactons (atoms:atomss) | (calcBBInteractions size atoms) == interactons = atoms
                                          | otherwise = testAtoms size interactons atomss

-- produces a list of subsets of a list of a given length
subsetsOfLengthI :: Int -> [a] -> [[a]]
subsetsOfLengthI 0 _ = [[]]

-- Base Case
subsetsOfLengthI _ [] = []

-- recurse on each element of the list, creating all possible sublists for each one
subsetsOfLengthI num (x : xs) = map (x :) (subsetsOfLengthI (num - 1) xs) ++ subsetsOfLengthI num xs

-- uses a grid of weighted positions and interactions and produces a list of attoms that satisfy that list of interactions
-- the grid of weighted positions is first sorted with highest weights first, then decomposed to just Positions which is then split
-- into a list of sublists of the same length as there are atoms. These lists are then tested one at a time and returned if it passes
generateAtomsFromNumberedGrid :: Int -> Int -> Interactions -> Grid -> Atoms
generateAtomsFromNumberedGrid numOfAtoms size interactions numberedGrid = testAtoms size interactions $ subsetsOfLengthI numOfAtoms (map (\((c,r),i) -> (c,r)) (reverse (sortBy (compare `on` (snd)) numberedGrid)))

-- takes a number of atoms aswell as a list of interactions and produces a list of atoms the satisfy the given list of interactions
solveBB :: Int -> Interactions -> Atoms
solveBB numOfAtoms interactions = generateAtomsFromNumberedGrid numOfAtoms size interactions (generateNumberedGrid size aplicableInteractions (generateEmptyGrid size))
                                  where size = ((length interactions) `div` 4)
                                        aplicableInteractions = (filter isApplicableHuristic interactions)

-- Challenge 3
-- Pretty Printing Lambda with Scott Numerals

data LamExpr =  LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int
                deriving (Eq, Show, Read)


isVar :: LamExpr -> Bool
isVar (LamVar x) = True
isVar _ = False

isScottNum :: LamExpr -> Bool
isScottNum (LamAbs x1 (LamAbs _ (LamVar x2))) | x1 == x2 = True
isScottNum (LamAbs x (LamAbs y1 (LamApp (LamVar y2) ( e1 ))))   | y1 == y2 = isScottNum e1
isScottNum _ = False


evalScottNum :: LamExpr -> Int -> Int
evalScottNum (LamAbs x1 (LamAbs _ (LamVar x2))) num = num
evalScottNum (LamAbs x (LamAbs y1 (LamApp (LamVar y2) ( e1 )))) num = evalScottNum e1 (num + 1)
evalScottNum _ _ = error "Cant Eval ScottNum"

-- takes a LamExpr and returns a equivelent expression which is easier to read ie prettyyyyy
-- Uses patteren match to decide if brackets are required, any brackets not needed are omited
-- As with the lambda calculus, application associates to the left and binds tighter than any other operator in each of the notations used here.
prettyPrint :: LamExpr -> String
prettyPrint (LamVar num) = "x" ++ show num
prettyPrint exp1@(LamAbs num lanExpr)   | isScottNum exp1 = show (evalScottNum exp1 0)
                                        | (not . isVar) lanExpr = "\\x" ++ show num ++ " -> " ++ prettyPrint lanExpr ++ ""
                                        | otherwise = "\\x" ++ show num ++ " -> " ++ prettyPrint lanExpr


prettyPrint (LamApp lanExpr1@(LamAbs x e1) lanExpr2)  | not (isVar lanExpr2) = "(" ++ prettyPrint lanExpr1 ++ ") " ++ prettyPrint lanExpr2
                                                      | otherwise = prettyPrint lanExpr1 ++ " " ++ prettyPrint lanExpr2

prettyPrint (LamApp lanExpr1 lanExpr2@(LamAbs x e1) ) | not (isVar lanExpr1) = "(" ++ prettyPrint lanExpr1 ++ ") " ++ prettyPrint lanExpr2
                                                      | otherwise = prettyPrint lanExpr1 ++ " " ++ prettyPrint lanExpr2

-- 2 Applications
prettyPrint (LamApp lanExpr1 lanExpr2)  | isVar lanExpr1 =  prettyPrint lanExpr1 ++ " " ++ prettyPrint lanExpr2
                                        | otherwise =  "(" ++ prettyPrint lanExpr1 ++ ") " ++ prettyPrint lanExpr2




-- Challenge 4
-- Parsing Let Expressions

data LetExpr =  LetApp LetExpr LetExpr | LetDef [([Int], LetExpr)] LetExpr |
                LetFun Int | LetVar Int | LetNum Int
                deriving (Show, Eq)


bracketParser :: Parser LetExpr
bracketParser = do
                space
                char '('
                space
                inside <- letAppParser <|> letDefParser <|> letFunParser <|> letVarParser
                space
                char ')'
                space
                return inside

-- atleast 2 LetExpr next to each other
letAppParser :: Parser LetExpr
letAppParser = do
              -- first part of App
              firstApplication <- bracketParser <|> letVarParser <|> letFunParser
              otherApplications <- some ( bracketParser <|> letVarParser <|> letFunParser)
              return (appFormatter (firstApplication:otherApplications))
              --if otherApplications == [] then return (LetApp firstApplication secondApplication) else return (LetApp (LetApp firstApplication secondApplication) (head otherApplications))

-- formats a list of LetExpr into a LetApp exprs. Uses leftHand recursion as LetExpr is left assoccitive
appFormatter :: [LetExpr] -> LetExpr
appFormatter (e1:e2:[]) = (LetApp e1 e2)
appFormatter es@(e:ess) = (LetApp (appFormatter (take allmostLength es)) ((drop allmostLength es)!!0) )
                            where allmostLength = ((length es) - 1)

getLetVarNum :: Parser Int
getLetVarNum = do
              space
              char 'x'
              num <- nat
              space
              return (num)

getFunNum :: Parser Int
getFunNum = do
              space
              char 'f'
              num <- nat
              space
              return (num)

letVarListParser :: Parser [Int]
letVarListParser = do
                vars <- many getLetVarNum
                space
                return vars

letFunThenVarListParser:: Parser [Int]
letFunThenVarListParser = do
                fun <- getFunNum
                var <-  getLetVarNum
                space
                return [fun, var]

-- parses the start of a let expression, allows for chaining with ';'
letSetsOfLetVars :: Parser ([Int], LetExpr)
letSetsOfLetVars = do
                  funAndVar <- letFunThenVarListParser
                  symbol "="
                  equalToVar <- letAppParser <|> letDefParser <|> letVarParser -- <|> letFunParser
                  symbol ";" <|> pure []
                  return (funAndVar, equalToVar)

letDefParser :: Parser LetExpr
letDefParser = do
              space
              symbol "let"
              accosiations <- many letSetsOfLetVars
              symbol "in"
              inExpr <- (letAppParser <|> (letDefParser <|> (letVarParser <|> (letFunParser)))) -- EDIT  ?
              return (LetDef accosiations inExpr)

letFunParser :: Parser LetExpr
letFunParser = do
              space
              char 'f'
              num <- nat
              space
              return (LetFun num)

letVarParser :: Parser LetExpr
letVarParser = do
              space
              char 'x'
              num <- nat
              space
              return (LetVar num)

letNumParser :: Parser LetExpr
letNumParser = do
              space
              num <- int
              space
              return (LetNum num)

-- attempts to parse a string into a LetExpr if the parse fails returns [] else Just the parsed LetExpr
parseLet :: String -> Maybe LetExpr
parseLet letExpr  | parseResult == [] = Nothing
                  | otherwise = Just ((fst . head) parseResult)
                  where parseResult = parse ( letAppParser <|> (letDefParser <|> (letFunParser <|> (letVarParser <|> (letNumParser))))) letExpr

-- Challenge 5
-- Encode lambda terms as combinators

data CLExpr = S | K  | I | CLVar Int | CLApp CLExpr CLExpr
              deriving (Show,Read,Eq)


clTransform :: LamExpr -> CLExpr
clTransform lamExpr = convert lamExpr

-- Checks if This Var Bound in a Abs AND Used
isBound :: Int -> LamExpr -> Bool -> Bool

isBound var (LamVar num) b  | var == num && b = True
                            | otherwise = False

isBound var (LamAbs num e1) b | var == num || b = isBound var e1 True
                              | otherwise = isBound var e1 False

isBound var (LamApp e1 e2) b = (isBound var e1 b) || (isBound var e2 b)


-- Checks if a var is used in the expression but not bound in it
freeOccurence :: Int -> LamExpr -> Bool -> Bool

freeOccurence var (LamVar num) b  | var == num && (not b) = True
                                  | otherwise = False

freeOccurence var (LamAbs num e1) b | var == num || b = freeOccurence var e1 True
                                    | otherwise = freeOccurence var e1 False

freeOccurence var (LamApp e1 e2) b = (freeOccurence var e1 b) || (freeOccurence var e2 b)


-- Checks if a var appears in a expression but is not bound
-- Since Were Are looking In A Allredy CLExpr We Are Essentiall Chechick If X Occurs Because if it does It will Allows Be Free
freeOccurenceCL :: Int -> CLExpr -> Bool -> Bool

freeOccurenceCL var (CLVar num) b   | var == num && (not b) = True
                                    | otherwise = False

freeOccurenceCL var K b = False
freeOccurenceCL var S b = False
freeOccurenceCL var I b = False
freeOccurenceCL var (CLApp e1 e2) b = (freeOccurenceCL var e1 b) || (freeOccurenceCL var e2 b)


-- special case that handles a half computated clExpr it takes a LamExpr part and a CLExpr part and produces a fully computated CLExpr
-- occurs when a expression in the form \x -> \y -> e if x occures free in e
specialCase :: LamExpr -> CLExpr -> CLExpr

-- <λ x → x>
-- I
specialCase lamExpr@(LamAbs var1 (LamVar _ )) (CLVar var2) | var1 == var2 = I

-- X Does Not Occur Free In E ( The K One )
-- <λ x → S I>
-- (K (S I))
specialCase lamExpr@(LamAbs var (LamVar _ )) clExpr | not (freeOccurenceCL var clExpr False) = (CLApp K clExpr)

-- The S One
-- <λ x → S I (K x)>
-- S <λ x → S I> <λ x → K x >
specialCase lamExpr@(LamAbs var (LamVar _ )) clExpr@(CLApp clExpr1 clExpr2) | (freeOccurenceCL var clExpr1 False) || (freeOccurenceCL var clExpr2 False) = (CLApp (CLApp S (specialCase lamExpr clExpr1) ) (specialCase lamExpr clExpr2) )


convert :: LamExpr -> CLExpr

-- Normal rule
convert (LamVar x) = (CLVar x)
convert (LamApp e1 e2) = (CLApp (convert e1) (convert e2) )

-- The I rule
convert ( LamAbs var (LamVar x)) | var == x = I

-- The K rule
convert ( LamAbs var e1 ) | not (freeOccurence var e1 False) = (CLApp K (convert e1))

-- The half LamExpr half CLEpr rule
convert ( LamAbs var1 e1@(LamAbs var2 e2)) | (freeOccurence var1 e2 False) = specialCase (LamAbs var1 (LamVar 0)) (convert (LamAbs var2 e2))

-- The S rule
convert ( LamAbs var (LamApp e1 e2) ) | (freeOccurence var e1 False) || (freeOccurence var e2 False) = (CLApp ( CLApp S (convert (LamAbs var e1))) (convert (LamAbs var e2))   )

-- Challenge 6
-- Compare Innermost and Outermost Reduction for Lambda and Combinators

-- checks if a var is free in a LamExpr expression
free :: Int -> LamExpr -> Bool
free lookingFor (LamVar var) = var == lookingFor
free lookingFor (LamApp e1 e2) = (free lookingFor e1) || (free lookingFor e2)
free lookingFor (LamAbs var e)  | lookingFor == var = False
                                | lookingFor /= var = free  lookingFor e

-- renames variables to a new name that is unsed / free, needed to aviod varible capture in Subsitution
alphaConvert :: Int -> LamExpr -> Int
alphaConvert var lamExpr  | free (var + 1) lamExpr = alphaConvert (var + 1) lamExpr
                          | otherwise = (var + 1)

-- Code segment adampted form Lecture 38 - Implementing Beta Reduction Slides, COMP2209 - Programming III, Dr Julian Rathke, University of Southampton

-- subsitutes all free vars "varToReplace" with "withExpr" in a given expression, uses alphaConvert to aviod vaible capture
subsitute :: Int -> LamExpr -> LamExpr -> LamExpr
subsitute varToReplace (LamVar var) withExpr | varToReplace == var = withExpr
subsitute varToReplace (LamVar var) withExpr | varToReplace /= var = (LamVar var)


subsitute varToReplace (LamAbs var lamExpr) withExpr  | varToReplace /= var && (not $ free var withExpr) = (LamAbs var (subsitute varToReplace lamExpr withExpr))
                                                      | varToReplace /= var && (free var withExpr) = let varRenamed = (alphaConvert var lamExpr) in
                                                                                                          subsitute varToReplace (LamAbs varRenamed (subsitute var lamExpr (LamVar varRenamed))) withExpr
                                                      | varToReplace == var = (LamAbs var lamExpr)-- no more free ocurences stop beta reduce
                                                      | otherwise = error "Subsitution error"

subsitute varToReplace (LamApp e1 e2) withExpr = (LamApp (subsitute varToReplace e1 withExpr) (subsitute varToReplace e2 withExpr))

-- Code Segment End

outerRedn1 :: LamExpr -> Maybe LamExpr

outerRedn1 (LamVar x) = Nothing
outerRedn1 (LamAbs var e) = Nothing
outerRedn1 (LamApp (LamVar var1) (LamVar var2)) = Nothing
outerRedn1 (LamApp (LamVar var1) (LamAbs var2 e1)) = Nothing

outerRedn1 lamExpr  = (Just (outerRedn1' lamExpr))

outerRedn1' :: LamExpr -> LamExpr
-- beta reduce
outerRedn1' (LamApp e@(LamAbs var1 e1) e2 ) = subsitute var1 e1 e2
outerRedn1' (LamApp e1 e2 ) | e1 == e1Res = (LamApp e1 (outerRedn1' e2))
                            | otherwise = (LamApp e1Res e2)
                              where e1Res = outerRedn1' e1

-- Converts a Just value to a value
maybeToNorm :: Eq a => Maybe a -> a
maybeToNorm (Just e) = e
maybeToNorm (Nothing) = error "Nothing to Just error"


-- implements inner most left most reduction
innerRedn1 :: LamExpr -> Maybe LamExpr

innerRedn1 (LamVar x) = Nothing
innerRedn1 (LamAbs var e) = Nothing
innerRedn1 (LamApp (LamVar var1) (LamVar var2)) = Nothing
innerRedn1 (LamApp (LamVar var1) (LamAbs var2 e1)) = Nothing

innerRedn1 lamExpr  = (Just (innerRedn1' lamExpr))

innerRedn1' :: LamExpr -> LamExpr

innerRedn1' (LamVar x) = (LamVar x)
innerRedn1' (LamAbs var e) = (LamAbs var e)

-- For Lam that Reduces To Its Self
innerRedn1' (LamApp l@(LamAbs var1 e1) (LamApp (LamAbs var2 e2) e@(LamAbs var3 e3)) ) = (LamApp l ( subsitute var2 e2 e ) )

innerRedn1' (LamApp e@(LamAbs var e1) e2) | e2Res == e2 = subsitute var e1 e2
                                          | otherwise= (LamApp e e2Res)
                                            where e2Res = innerRedn1' e2

innerRedn1' e@(LamApp e1 e2 ) | e1Res == e1 = (LamApp e1 (innerRedn1' e2))
                              | otherwise = (LamApp e1Res e2)
                                where e1Res = innerRedn1' e1


outerCLRedn1 :: CLExpr -> Maybe CLExpr
outerCLRedn1 I = Nothing
outerCLRedn1 K = Nothing
outerCLRedn1 S = Nothing
outerCLRedn1 (CLVar var) = Nothing

outerCLRedn1 (CLApp I e1) = (Just e1)
outerCLRedn1 (CLApp (CLApp K e1) e2) = (Just e1)
outerCLRedn1 (CLApp (CLApp (CLApp S e1) e2) e3) = (Just (CLApp (CLApp e1 e3) (CLApp e2 e3)))
outerCLRedn1 (CLApp e1 e2)  | e1Res == e1 && e2Res == e2 = Nothing
                            | e1Res /= e1 && e2Res == e2 = (Just (CLApp e1Res e2))
                            | e1Res == e1 && e2Res /= e2 = (Just (CLApp e1 e2Res))
                            | e1Res /= e1 && e2Res /= e2 = (Just (CLApp e1Res e2))
                              where e1Res = if (outerCLRedn1 e1) == Nothing then e1 else maybeToNorm (outerCLRedn1 e1)
                                    e2Res = if (outerCLRedn1 e2) == Nothing then e2 else maybeToNorm (outerCLRedn1 e2)


innerCLRedn1 :: CLExpr -> Maybe CLExpr

innerCLRedn1 I = Nothing
innerCLRedn1 K = Nothing
innerCLRedn1 S = Nothing
innerCLRedn1 (CLVar var) = Nothing

innerCLRedn1 e  | e == eRes = Nothing
                | otherwise = (Just eRes)
                  where eRes = innerCLRedn1' e

-- implements CL reduction rules for innermost
innerCLRedn1' :: CLExpr -> CLExpr
innerCLRedn1' (CLApp I e1) | e1 == e1Res = e1
                           | otherwise = (CLApp I (e1Res))
                            where e1Res = innerCLRedn1' e1

innerCLRedn1' (CLApp (CLApp K e1) e2) | e1 == e1Res && e2 == e2Res = e1
                                      | e1 == e1Res && e2 /= e2Res = (CLApp (CLApp K e1) e2Res)
                                      | e1 /= e1Res && e2 == e2Res = (CLApp (CLApp K e1Res) e2)
                                      | otherwise =  (CLApp (CLApp K e1Res) e2)
                                        where e1Res = innerCLRedn1' e1
                                              e2Res = innerCLRedn1' e2

innerCLRedn1' (CLApp (CLApp (CLApp S e1) e2) e3)  | e1 == e1Res && e2 == e2Res && e3 == e3Res = (CLApp (CLApp e1 e3) (CLApp e2 e3))
                                                  | e1 /= e1Res && e2 == e2Res && e3 == e3Res = (CLApp (CLApp (CLApp S e1Res) e2) e3)
                                                  | e1 == e1Res && e2 /= e2Res && e3 == e3Res = (CLApp (CLApp (CLApp S e1) e2Res) e3)
                                                  | e1 == e1Res && e2 == e2Res && e3 /= e3Res = (CLApp (CLApp (CLApp S e1) e2) e3Res)
                                                  | e1 /= e1Res && e2 /= e2Res && e3 == e3Res = (CLApp (CLApp (CLApp S e1Res) e2) e3)
                                                  | e1 == e1Res && e2 /= e2Res && e3 /= e3Res = (CLApp (CLApp (CLApp S e1) e2Res) e3)
                                                  | e1 /= e1Res && e2 == e2Res && e3 /= e3Res = (CLApp (CLApp (CLApp S e1Res) e2) e3)
                                                  | otherwise = (CLApp (CLApp (CLApp S e1Res) e2) e3)
                                                    where e1Res = innerCLRedn1' e1
                                                          e2Res = innerCLRedn1' e2
                                                          e3Res = innerCLRedn1' e3--}

innerCLRedn1' (CLApp e1 e2) | e1 == e1Res && e2 == e2Res = (CLApp e1 e2)
                            | e1 == e1Res && e2 /= e2Res = (CLApp e1 e2Res)
                            | e1 /= e1Res && e2 == e2Res = (CLApp e1Res e2)
                            | otherwise =  (CLApp e1Res e2)
                              where e1Res = innerCLRedn1' e1
                                    e2Res = innerCLRedn1' e2

-- Desnt Reduce
innerCLRedn1' clExpr = clExpr


compareInnerOuter :: LamExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter lamExpr bound = (inner, outer, clInner, clOuter)
                        where inner = countReductionsNum lamExpr bound 0 innerRedn1
                              outer = countReductionsNum lamExpr bound 0 outerRedn1
                              clInner = countReductionsNum (convert lamExpr) bound 0 innerCLRedn1
                              clOuter = countReductionsNum (convert lamExpr) bound 0 outerCLRedn1

-- takes a expression, a bound, steps allready copmuted and a evaluation meathod
-- it applies the given expression to the evaluation meathod while the steps taken so far
-- doesnt exceed the bound, if no it returns Nothing. If the expression cant be reduced any
-- further Just stepsSoFar is returned
countReductionsNum :: Eq a => a -> Int -> Int -> (a -> Maybe a) -> Maybe Int
countReductionsNum expr bound stepsSoFar evalMeathodStep  | (bound < 0) = Nothing
                                                          | nextStep == Nothing = (Just stepsSoFar)
                                                          | otherwise = countReductionsNum (maybeToNorm nextStep) (bound - 1) (stepsSoFar + 1) evalMeathodStep
                                                            where nextStep = evalMeathodStep expr --}
