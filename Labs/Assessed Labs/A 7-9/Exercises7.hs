{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (Expr(..),toNNF) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Exercise A7

data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord)


--Define Expr as an instance of Show here:

instance Show Expr  where
--instance (Expr expr) => show expr where
  --show (Not (And (Not (Var 'a')) (Var 'b'))) =  "~(~a ^ b)"
  show (Var c) =  [c] --
  show (Not (Var x)) = ("~" ++ [x]) --
  --show (And (Var a) (Var b)) = ( [a] ++ " ^ " ++ [b])
  --show (Or (Var a) (Var b)) = ( [a] ++ " v " ++ [b])
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

--
  show (Or (Var a) (Or expr2 expr3)) = ( "(" ++ [a] ++ " v " ++ "(" ++ (show expr2) ++ " v " ++ (show expr3) ++ "))") --
  show (Or (Var a) (And expr2 expr3)) = ( "(" ++ [a] ++ " v " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ "))")
  show (Or (Not (Var a)) (And expr2 expr3)) = ( "(~" ++ [a] ++ " v " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ "))")
  show (Or (And expr2 expr3) (Var a) ) = ( "(" ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ ") v " ++ [a] ++ ")")
  show (Or (And expr2 expr3) (Not (Var a)) ) = ( "(" ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ ") v ~" ++ [a] ++ ")")

  show (Or (Not (Var a)) (Var b)) = ( "~" ++ [a] ++ " v " ++ [b] )
  show (Or (Not (Var a)) (Not (Var b))) = ( "~" ++ [a] ++ " v ~" ++ [b] )

  show (Or (Var a) (Var b)) = ( [a] ++ " v " ++ [b])
  show (Or (Var a) (Not(Var b))) = ( [a] ++ " v ~" ++ [b])


{-
  show (Or (Var a) (Var b)) = ( [a] ++ " v " ++ [b] )
  show (Or (Var a) (Or expr2 expr3)) = ( "(" ++ [a] ++ " ^ " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ "))")
  show (And expr1 (And expr2 expr3)) = ( "(" ++ show expr1 ++ ") ^ " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ ")")
  show (Or expr1 (Or expr2 expr3)) = ( "(" ++ show expr1 ++ ") ^ " ++ "(" ++ (show expr2) ++ " ^ " ++ (show expr3) ++ ")")
-}

  show (And expr1 expr2) = ( "(" ++ show expr1 ++ ") ^ (" ++ show expr2 ++ ")" )
  show (Or expr1 expr2) = ( "(" ++ show expr1 ++ ") v (" ++ show expr2 ++ ")" )

  show (Not(expr)) = ("~(" ++ show expr ++ ")")

{-
  show (And (Var a) expr) = ( [a] ++ " ^ (" ++ show expr ++ ")")
  show (Or (Var a) expr) = ( [a] ++ " v (" ++ show expr ++ ")" )
  show (And expr (Var b)) = ( "(" ++ show expr ++ ") ^ " ++ [b])
  show (Or expr (Var b)) = ( "(" ++ show expr ++ ") v " ++ [b] )
---}




  --show (Not (Or expr1 expr2)) = ( "~(" ++ show expr1 ++ " v " ++ show expr2 ++ ")")
--  show (Not (And expr1 expr2)) = ( "~(" ++ show expr1 ++ " ^ " ++ show expr2 ++ ")") --


---}
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
