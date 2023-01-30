{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (Expr(..),toNNF,Binding,Interpretation,consistent,solve,satisfiable,maxSatisfiable) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List
import Data.Ord

-- Exercise A7
data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord)

--Expr(..),toNNF,Binding,Interpretation,consistent,solve,satisfiable,maxSatisfiable) where
--stages,nnfStep,toDNF,removeSuperLists,removeDuplicateAssigments,checkIfSuperset,contains,remove,solve2,satisfiable',merge2,powerset,findLongestList,getBiggestSet,getBiggestSet',removeItem

stages :: Expr -> [(Expr,Expr)]
stages e = [ p | p <- zip evals (tail evals) ]
          where evals = iterate nnfStep e

toNNF :: Expr -> Expr
toNNF = fst . head . dropWhile (\(a,b) -> a /= b) . stages

nnfStep :: Expr -> Expr
nnfStep (Not(Or expr1 expr2)) = (And ( nnfStep (Not expr1) ) ( nnfStep (Not expr2) ))
nnfStep (Not(And expr1 expr2)) = (Or ( nnfStep (Not expr1) ) ( nnfStep (Not expr2) ))
nnfStep (Not(Not expr)) = expr
nnfStep (Not (Var x)) = (Not (Var x))
nnfStep x = x

instance Show Expr  where

  show (Var c) =  [c] --
  show (Not (Var x)) = ("~" ++ [x]) --
  show (Not(Not (Var a))) = ("~~" ++ [a])
  show (Not(Not expr)) = ("~~(" ++ show expr ++ ")")

  show (And (Var a) (And expr2 expr3)) = ( "(" ++ [a] ++ " ^ " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ "))")
  show (And (Not (Var a)) (And expr2 expr3)) = ( "(~" ++ [a] ++ " ^ " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ "))")
  show (And (And expr2 expr3) (Var a) ) = ( "(" ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ ") ^ " ++ [a] ++ ")")
  show (And (And expr2 expr3) (Not (Var a)) ) = ( "(" ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ ") ^ ~" ++ [a] ++ ")")

  show (And (Not (Var a)) (Var b)) = ( "~" ++ [a] ++ " ^ " ++ [b] )
  show (And (Not (Var a)) (Not (Var b))) = ( "~" ++ [a] ++ " ^ ~" ++ [b] )

  show (And (Var a) (Var b)) = ( [a] ++ " ^ " ++ [b])
  show (And (Var a) (Not(Var b))) = ( [a] ++ " ^ ~" ++ [b])

  show (Or (Var a) (Or expr2 expr3)) = ( "(" ++ [a] ++ " v " ++ "(" ++ (show expr2) ++ " v " ++ (show expr3) ++ "))") --
  show (Or (Var a) (And expr2 expr3)) = ( "(" ++ [a] ++ " v " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ "))")
  show (Or (Not (Var a)) (And expr2 expr3)) = ( "(~" ++ [a] ++ " v " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ "))")
  show (Or (And expr2 expr3) (Var a) ) = ( "(" ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ ") v " ++ [a] ++ ")")
  show (Or (And expr2 expr3) (Not (Var a)) ) = ( "(" ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ ") v ~" ++ [a] ++ ")")

  show (Or (Not (Var a)) (Var b)) = ( "~" ++ [a] ++ " v " ++ [b] )
  show (Or (Not (Var a)) (Not (Var b))) = ( "~" ++ [a] ++ " v ~" ++ [b] )

  show (Or (Var a) (Var b)) = ( [a] ++ " v " ++ [b])
  show (Or (Var a) (Not(Var b))) = ( [a] ++ " v ~" ++ [b])

  show (And expr1 expr2) = ( "(" ++ show expr1 ++ ") ^ (" ++ show expr2 ++ ")" )
  show (Or expr1 expr2) = ( "(" ++ show expr1 ++ ") v (" ++ show expr2 ++ ")" )

  show (Not(expr)) = ("~(" ++ show expr ++ ")")


-- Exercise A8
type Binding = (Char, Bool)
type Interpretation = [Binding]

checkBinging :: Binding -> Interpretation -> Bool
checkBinging b [] = True
checkBinging b@(c1, a1) ((c2, a2):xs) | c1 == c2 && a1 == a2 = checkBinging b xs
                                    | c1 == c2 = False
                                    | otherwise = checkBinging b xs

consistent :: Interpretation -> Bool
consistent i = foldr (&&) True [ checkBinging b i | b <- i ]

toDNF :: Expr -> Expr -- From a NNF
toDNF (And e1 e2) = dist (toDNF e1) (toDNF e2)
                    where
                      dist :: Expr -> Expr -> Expr
                      dist (Or e11 e12) e2 = Or (e11 `dist` e2)  (e12 `dist` e2)
                      dist e1 (Or e21 e22) = Or (e1 `dist` e21)  (e1 `dist` e22)
                      dist e1 e2                    = And e1 e2

toDNF (Or e1 e2) = Or (toDNF e1) (toDNF e2)
toDNF (expr) = expr

solve :: Expr -> [Interpretation]
solve expr = removeDuplicateAssigments $ removeSuperLists $  filter consistent (solve' (toDNF $ toNNF expr))
            where
              solve' :: Expr -> [Interpretation]
              solve' (Var x) = [[(x,True)]]
              solve' (Not(Var x)) = [[(x,False)]]

              solve' (And e1 e2) =  [ (solve'' e1) ++ (solve'' e2) ]

              solve' (Or (Or e1 e2) (Var x)) = (solve'' e1) : (solve'' e2) : [[(x,True)]] -- not/var/and var
              solve' (Or (Or e1 e2) (Not(Var x))) = (solve'' e1) : (solve'' e2) : [[(x,False)]] -- not/var/and not
              solve' (Or (Or e1 e2) (And e3 e4)) = (solve'' e1) : (solve'' e2) : (solve'' (And e3 e4)) : [] -- not/var/and and

              solve' (Or e1 (Var x)) = (solve'' e1) : [[(x,True)]] -- not/var/and var
              solve' (Or e1 (Not(Var x))) = (solve'' e1) : [[(x,False)]] -- not/var/and not
              solve' (Or e1 (And e2 e3)) = (solve'' e1) : (solve'' (And e2 e3)) : [] -- not/var/and and

              solve' (Or e1 e2) = (solve'' e1) : (solve' e2) -- or or Inter : [Inter]

              solve'' :: Expr -> Interpretation
              solve'' (Var x) = [(x,True)]
              solve'' (Not(Var x)) = [(x,False)]

              solve'' (And (And e1 e2) (Var x)) =  (solve''' e1) : (solve''' e2) : (x,True) : []-- and var T
              solve'' (And (And e1 e2) (Not (Var x))) = (solve''' e1) : (solve''' e2) : (x,False) : []  -- and not T
              solve'' (And (And e1 e2) e3) = (solve''' e1) : (solve''' e2) : (solve'' e3) -- and and

              solve'' (And e1 (Var x)) = (solve''' e1) : (x,True) : []
              solve'' (And e1 (Not (Var x))) = (solve''' e1) : (x,False) : []
              solve'' (And e1 e2) = (solve''' e1) : (solve'' e2) -- not/var and


              solve''' :: Expr -> Binding -- and / var / NotVar
              solve''' (Not(Var x)) = (x, False)
              solve''' (Var x) = (x, True)

              --solve'''

removeSuperLists :: [Interpretation] -> [Interpretation]
removeSuperLists inter = remove inter [ inter1 | inter1 <- inter, inter2 <- inter, inter1 /= inter2, (checkIfSuperset inter1 inter2) ]

removeDuplicateAssigments ::  [Interpretation] -> [Interpretation]
removeDuplicateAssigments = map (map head . group . sort)

checkIfSuperset :: Interpretation -> Interpretation -> Bool -- If this a superset of this
checkIfSuperset inter1 inter2 = if (length [ b | b <- inter2, (contains inter1 b)]) == length inter2 then True else False

contains :: Interpretation -> Binding -> Bool
contains [] _ = False
contains (b1@(c1,a1):bs) b2@(c2,a2) | b1 == b2 = True
                                     | otherwise = contains bs b2

--contains :: [Interpretation] -> Interpretation

remove :: [Interpretation] -> [Interpretation] -> [Interpretation] -- remove from this, these
remove is ris = [inter | inter <- is, (not $ elem inter ris)]


solve2 :: Expr -> [Interpretation]
solve2 expr = filter consistent (solve' (toDNF $ toNNF expr))
            where
              solve' :: Expr -> [Interpretation]
              solve' (Var x) = [[(x,True)]]
              solve' (Not(Var x)) = [[(x,False)]]

              solve' (And e1 e2) =  [ (solve'' e1) ++ (solve'' e2) ]

              solve' (Or (Or e1 e2) (Var x)) = (solve'' e1) : (solve'' e2) : [[(x,True)]] -- not/var/and var
              solve' (Or (Or e1 e2) (Not(Var x))) = (solve'' e1) : (solve'' e2) : [[(x,False)]] -- not/var/and not
              solve' (Or (Or e1 e2) (And e3 e4)) = (solve'' e1) : (solve'' e2) : (solve'' (And e3 e4)) : [] -- not/var/and and

              solve' (Or e1 (Var x)) = (solve'' e1) : [[(x,True)]] -- not/var/and var
              solve' (Or e1 (Not(Var x))) = (solve'' e1) : [[(x,False)]] -- not/var/and not
              solve' (Or e1 (And e2 e3)) = (solve'' e1) : (solve'' (And e2 e3)) : [] -- not/var/and and

              solve' (Or e1 e2) = (solve'' e1) : (solve' e2) -- or or Inter : [Inter]

              solve'' :: Expr -> Interpretation
              solve'' (Var x) = [(x,True)]
              solve'' (Not(Var x)) = [(x,False)]

              solve'' (And (And e1 e2) (Var x)) =  (solve''' e1) : (solve''' e2) : (x,True) : []-- and var T
              solve'' (And (And e1 e2) (Not (Var x))) = (solve''' e1) : (solve''' e2) : (x,False) : []  -- and not T
              solve'' (And (And e1 e2) e3) = (solve''' e1) : (solve''' e2) : (solve'' e3) -- and and

              solve'' (And e1 (Var x)) = (solve''' e1) : (x,True) : []
              solve'' (And e1 (Not (Var x))) = (solve''' e1) : (x,False) : []
              solve'' (And e1 e2) = (solve''' e1) : (solve'' e2) -- not/var and


              solve''' :: Expr -> Binding -- and / var / NotVar
              solve''' (Not(Var x)) = (x, False)
              solve''' (Var x) = (x, True)

satisfiable :: [Expr] -> Bool
satisfiable [] = True
satisfiable [x] = if (length $ solve2 x) /= 0 then True else False
satisfiable exprs = satisfiable' (solve2 $ head exprs) (tail [ solve2 (toDNF $ toNNF expr) | expr <- exprs])


satisfiable' :: [Interpretation] -> [[Interpretation]] -> Bool
satisfiable' _ [] = True -- IF we make merged all the interperatiations without faliure
satisfiable' runningInter (inter1:inters) | newRunningInter == [] = False -- if a mergre creates an empty interpertation
                                          | otherwise = satisfiable' newRunningInter inters -- merge with the next interpertation
                                           where newRunningInter = (merge2 inter1 runningInter) --- THink About Me!  intersect inter1 runningInter

merge2 :: [Interpretation] -> [Interpretation] -> [Interpretation]
merge2 xs ys = [ head $ removeDuplicateAssigments [(x++y)] | x <- xs, y <- ys, consistent (x++y)]

-- Exercise A9

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

findLongestList :: [[a]] -> Int
findLongestList lists = length $ maximumBy (comparing length) lists

getBiggestSet :: [[a]] -> [[a]]
getBiggestSet [] = [[]]
getBiggestSet lists = getBiggestSet' lists [] (findLongestList lists)

getBiggestSet' :: [[a]] -> [[a]] -> Int -> [[a]]
getBiggestSet' [] acc len = acc
getBiggestSet' (x:xs) acc len = if (length x) == len then getBiggestSet' xs (x : acc) len  else getBiggestSet' xs acc len


removeItem :: Eq a =>  a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys


maxSatisfiable :: [Expr] -> [[Expr]]
maxSatisfiable [] = [[]]
maxSatisfiable exprs = getBiggestSet (filter (\x -> satisfiable x == True) (removeItem [] (powerset exprs)))
