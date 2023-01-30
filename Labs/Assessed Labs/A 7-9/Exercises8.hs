{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (Expr(..),Binding,Interpretation,consistent,solve,satisfiable) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List


-- Exercise A8
data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord,Show)
--data DNFExpr = Var Char | Not Var | And Expr Expr | Or And Or | Or And And deriving (Eq, Ord,Show)
{-
instance Show Expr  where
--instance (Expr expr) => show expr where
  show (Var c) =  [c] --
  show (Not (Var x)) = ("~" ++ [x]) --
  --show (And (Var a) (Var b)) = ( [a] ++ " ^ " ++ [b])
  --show (Or (Var a) (Var b)) = ( [a] ++ " v " ++ [b])
  show (Not(Not (Var a))) = ("~~" ++ [a])
  show (Not(Not expr)) = ("~~(" ++ show expr ++ ")")

  show (And (Var a) (Var b)) = ( [a] ++ " ^ " ++ [b])
  show (Or (Var a) (Var b)) = ( [a] ++ " v " ++ [b] )

  show (And (Var a) (And expr2 expr3)) = ( "(" ++ [a] ++ " ^ " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ "))")
  show (Or (Var a) (Or expr2 expr3)) = ( "(" ++ [a] ++ " ^ " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ "))")
  show (And expr1 (And expr2 expr3)) = ( "(" ++ show expr1 ++ ") ^ " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ ")")
  show (Or expr1 (Or expr2 expr3)) = ( "(" ++ show expr1 ++ ") ^ " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ ")")

  show (And expr1 expr2) = ( "(" ++ show expr1 ++ ") ^ (" ++ show expr2 ++ ")" )
  show (Or expr1 expr2) = ( "(" ++ show expr1 ++ ") v (" ++ show expr2 ++ ")" )



{-
  show (And (Var a) expr) = ( [a] ++ " ^ (" ++ show expr ++ ")")
  show (Or (Var a) expr) = ( [a] ++ " v (" ++ show expr ++ ")" )
  show (And expr (Var b)) = ( "(" ++ show expr ++ ") ^ " ++ [b])
  show (Or expr (Var b)) = ( "(" ++ show expr ++ ") v " ++ [b] )
---}


  show (Not(expr)) = ("~(" ++ show expr ++ ")")

  ---}
type Binding = (Char, Bool)
type Interpretation = [Binding]

checkBinging :: Binding -> Interpretation -> Bool
checkBinging b [] = True
checkBinging b@(c1, a1) ((c2, a2):xs) | c1 == c2 && a1 == a2 = checkBinging b xs
                                    | c1 == c2 = False
                                    | otherwise = checkBinging b xs

consistent :: Interpretation -> Bool
consistent i = foldr (&&) True [ checkBinging b i | b <- i ]




-- Define toNNF here:
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


toDNF :: Expr -> Expr -- From a NNF
toDNF (And e1 e2) = dist (toDNF e1) (toDNF e2)
                    where
                      dist :: Expr -> Expr -> Expr
                      dist (Or e11 e12) e2 = Or (e11 `dist` e2)  (e12 `dist` e2)
                      dist e1 (Or e21 e22) = Or (e1 `dist` e21)  (e1 `dist` e22)
                      dist e1 e2                    = And e1 e2

toDNF (Or e1 e2) = Or (toDNF e1) (toDNF e2)
toDNF (expr) = expr

{-
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs
-}

--allPossibleInter :: Expr -> [Interpretation]
--allPossibleInter e = [ | v <- getAllUniqueVars]

--(:') :: [a] -> a ->

solve :: Expr -> [Interpretation]
solve expr = removeDuplicateAssigments $ removeSuperLists $  filter consistent (solve' (toDNF $ toNNF expr))
            where
              solve' :: Expr -> [Interpretation]
              solve' (Var x) = [[(x,True)]]
              solve' (Not(Var x)) = [[(x,False)]]

              {-
              solve' (And (Var a) (Var b)) =  [[(a,True),(b,True)]]
              solve' (And (Var a) (Not(Var b))) =  [[(a,True),(b,False)]]
              solve' (And (Not(Var a)) (Var b)) =  [[(a,False),(b,True)]]
              solve' (And (Not(Var a)) (Not(Var b))) =  [[(a,False),(b,False)]]

              solve' (And e1 (Var b)) =  (solve'' e1) : [(b,True)] : []
              solve' (And e1 (Not(Var b))) =  (solve'' e1) : [(b,False)] : []

              solve' (And e1 e2) =  (solve'' e1) : (solve' e2)
              --}

              solve' (And e1 e2) =  [ (solve'' e1) ++ (solve'' e2) ]

              solve' (Or (Or e1 e2) (Var x)) = (solve'' e1) : (solve'' e2) : [[(x,True)]] -- not/var/and var
              solve' (Or (Or e1 e2) (Not(Var x))) = (solve'' e1) : (solve'' e2) : [[(x,False)]] -- not/var/and not
              solve' (Or (Or e1 e2) (And e3 e4)) = (solve'' e1) : (solve'' e2) : (solve'' (And e3 e4)) : [] -- not/var/and and
              --solve' (Or (Or e1 e2) (Var x)) = (solve'' e1) : (solve'' e2) : [[(x,True)]]   --
              --solve' (Or (Or e1 e2) (Not(Var x))) = (solve'' e1) : (solve'' e2) : [[(x,False)]]  --
              --solve'' (Or e1 e2) = (solve'' e1) ++ (solve'' e2)----------

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
--remove [] _ = []
--remove is [] = is
remove is ris = [inter | inter <- is, (not $ elem inter ris)]

{-
solve = solve2 . toNNF
solve (Or e1 e2) = (solv e1) : (solv e1) : []
solve (And e1 e2) = (solv e1) ++ (solve e2) : []
solve (Not e1) =
solve (Var x) = [[(x,True)]]

solve (Or e1 e2) currInter allInter = (solve e2 currInter) ++ (solve e1 currInter) ++ allInter
solve (And e1 e2) currInter allInter = [ interOfe1 ++ interOfe2 | interOfe1 <- (solve e1 currInter), interOfe2 <- (solve e2 currInter) ]] ++ allInter
solve (Not (Var x))
---}

{-
uniq :: Eq a => [a] -> [a] -> [a]
uniq x [] = x
uniq [] (a:xs) = uniq [a] xs
uniq x (a:xs) = if a `elem` x then uniq x xs else uniq (a:x) xs


getAllUniqueVars :: Expr -> String
getAllUniqueVars e = uniq [] $ sort $ getAllVars e

getAllVars :: Expr -> String
getAllVars (Var x) = [x]
getAllVars (Or e1 e2) = (getAllVars e1) ++ (getAllVars e2)
getAllVars (And e1 e2) = (getAllVars e1) ++ (getAllVars e2)
getAllVars (Not e1) = (getAllVars e1)
-}

{-
solv:: Expr -> Interpretation
sovle (Var x) = [(x,True)]

solveHelper :: Expr -> [Interpretation] -> [Interpretation]
solveHelper (And e1 e2) is = ():is
solveHelper (Var x) is = [ (x,True):i | i <- is]
solverHelper (And e1 e2) i = sovlerHelper
-}
{-
solveHelper :: Expr -> Interpretation -> Interpretation
solveHelper (Var x) i = if (checkBinging (x,True) i) then (x,True):i else []
solverHelper (And e1 e2) i = sovlerHelper
--}
{--
solve2 :: Expr -> [Interpretation]
solve2 expr = filter consistent (solve' (toDNF $ toNNF expr))
            where
              solve' :: Expr -> [Interpretation]
              solve' (Or e1 (And e2 e3)) = (solve'' e1) : (solve'' (And e2 e3)) : []
              solve' (Or e1 e2) = (solve'' e1) : (solve' e2)

              solve'' :: Expr -> Interpretation
              solve'' (Var x) = [(x,True)]
              solve'' (Not(Var x)) = [(x,False)]
              solve'' (And e1 (Var x)) = (solve''' e1) : (x,True) : []
              solve'' (And e1 (Not (Var x))) = (solve''' e1) : (x,False) : []
              solve'' (And e1 e2) = (solve''' e1) : (solve'' e2)

              solve''' :: Expr -> Binding
              solve''' (Not(Var x)) = (x, False)
              solve''' (Var x) = (x, True)
--}

solve2 :: Expr -> [Interpretation]
--removeDuplicateAssigments $ removeSuperLists $  filter consistent (solve' (toDNF $ toNNF expr))
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
satisfiable exprs = satisfiable' (solve2 $ head exprs) (tail [ solve2 (toDNF $ toNNF expr) | expr <- exprs])


satisfiable' :: [Interpretation] -> [[Interpretation]] -> Bool

satisfiable' _ [] = True -- IF we make merged all the interperatiations without faliure
satisfiable [x] = if (length $ solve2 x) /= 0 then True else False
satisfiable' runningInter (inter1:inters) | newRunningInter == [] = False -- if a mergre creates an empty interpertation
                                          | otherwise = satisfiable' newRunningInter inters -- merge with the next interpertation
                                           where newRunningInter = (merge2 inter1 runningInter) --- THink About Me!  intersect inter1 runningInter

{-
mergeInterpretations :: [Interpretation] -> [Interpretation] -> [Interpretation]
mergeInterpretations inter1 inter2
-}


--Just check if 1 is a super set of another


merge2 :: [Interpretation] -> [Interpretation] -> [Interpretation]
merge2 xs ys = [ head $ removeDuplicateAssigments [(x++y)] | x <- xs, y <- ys, consistent (x++y)]

merge :: [Interpretation] -> [Interpretation] -> [Interpretation]
--merge (x:xs) ys | foldr (||)  Flase (map find x ys) == True = x : merge ys --- for each inter in i1 see if it matches a inter form i2
merge [] _ = []
merge (x:xs) ys = (findI x ys) ++ (merge xs ys) --- for each inter in i1 see if it matches a inter form i2
--merge inters1 inters2 = [  | inter1 <- inters1, findI inter1 inters2]


findI :: Interpretation -> [Interpretation] -> [Interpretation] -- given an interpertation and a list of interpertations
-- check each interpertation in the list to check if it is a superset of the given interpertation,
-- OR the given interperation is a superset of it
findI inter1 inters = [ inter1 | inter2 <- inters, checkIfSuperset inter1 inter2 ] ++ [ inter2 | inter2 <- inters, checkIfSuperset inter2 inter1]
















--
