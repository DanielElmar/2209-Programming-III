--EX1

import Data.Graph
import Data.Array

{-
nodes :: [Vertex]
nodes = [ n | n <- [0..1000] ]

edges :: [Edge]
edges = [ (a,(a+1)) | a <- [0,2..999] ] ++ [ (a,(a `div` 5)) | a <- [1,3..1000], ((a `mod` 5) == 0) ]

connectedVertices :: Vertex -> [Edge] -> [Vertex]
connectedVertices a b = map snd $ filter (\(u,v) -> u==a) b

bounds :: Bounds
bounds = (0,1000)

graph :: Graph
graph = array Main.bounds [ (a,connectedVertices a Main.edges) | a <-nodes ]
-}


evenedges = [ (n,n+1) | n <-[0,2..998] ]
oddedges = [ (n, n `div` 5) | n <- [1,3..999] ]

graph = buildG (0,1000) (evenedges++oddedges)

isReachable :: Int -> Int -> Bool
isReachable n m = elem m (reachable graph n)

--EX2


data GGraph a = GNode a (GGraph a) deriving Show

mkGraph :: [ (a, Int) ]-> [GGraph a]
mkGraph table = table'
 where --table' :: [GGraph a]
       table' = map (\(x,n) -> GNode x (table'!!n)) table

table = merge evenedges oddedges
  where merge (x:xs) ys = x : (merge ys xs)
        merge [] ys = ys

graphCD = mkGraph table


nextNode :: GGraph a -> GGraph a
nextNode (GNode a g) = g

nodeID :: GGraph a -> a
nodeID (GNode v g) = v

isReachableCD :: Int -> Int -> Bool
isReachableCD n m = isReachableCD' (graphCD!!n) m [n]
  where isReachableCD' node m visited | (nodeID node) == m = True
        isReachableCD' node m visited | elem (nodeID node) visited = False 
        isReachableCD' node m visited | otherwise = isReachableCD' (nextNode node) m ((nodeID node):visited)

--EX 3
{-

Define the append function as
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys) 


use equational reasoning to show that
1)  xs ++ [ ] = xs
2)  xs ++ (ys ++ zs) = (xs ++ ys) ++ zs.


1)
Reason by induction on List x
Base case, xs is []
[] ++ [] = []  by defintion  ->

Inductive case, suppose xs ++ [] = xs
(x:xs) ++ [] 
= x : (xs ++ [])    by definition ->
= x : xs  by the inductive hypothesis

2)
Reason by induction on List x
Base case, xs is []
[] ++ (ys ++ zs) 
  = ys ++ zs  by definition ->
  =  ([] ++ ys) ++ zs  by definition <-

 Inductive case, suppose xs ++ (ys ++ zs) = (xs + ys) ++zs
 (x:xs) ++ (ys ++ zs) 
  = x : (xs ++ (ys ++ zs))  by definition ->
  = x : ((xs ++ ys) ++ zs)  by the inductive hypothesis
  = (x : (xs ++ ys))  ++ zs by definition <-
  = ((x:xs) ++ ys) ++ zs by definition <-

-}

--EX 4
{-

data Nat = Zero | Succ Nat


replicate :: Nat -> a -> [a]
replicate Zero _ = []
replicate (Succ n) x = x : replicate n x

We wish to prove that replicate produces a list whose elements are all equal to the element given to replicate. To do this we can define the function

all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) = p x && all p xs

and prove that all (== x) (replicate n x) = True for any x and any n. Write this proof. What assumptions do you need to make about (==) in order for the proof to hold? Are these valid assumptions?

We assume == is able of operate on type x / x is of class Eq

Base Case, n is Zero
all (== x) (replicate Zero x)
= all (== x) []  by definition of replicate ->
= True  by definition of all ->

Inductive case, suppose: all (== x) (replicate n x) = True


all (== x) (replicate (Succ n) x)
  = all (==x) ( x : replicate n x)		By definition of Replicate
  = (==x) x && all (==x) (replicate n x)	By definition of all
  = True && all (==x) (replicate n x)		
  = all (==x) (replicate n x)			Boolean simplification
  = True					By inductive hypohisis

-}
--EX 5

{-
Prove that for any list we have take n xs ++ drop n xs = xs where

 take Zero _  = []						 
 take _ [] = []							  
 take (Succ n) (x:xs) = x : take n xs

 drop Zero xs = xs
 drop _ [] = []
 drop (Succ n) (_:xs) = drop n	xs


Base Case n is 0

take n xs ++ drop n xs
  = [] ++ drop n xs 	By definition of take
  = [] ++ xs 		By definition od drop
  = xs 			By definition of ++

Inductive case, supose for all n take n xs ++ drop n xs = xs
inductive hypotinsis take (Succ n) xs ++ drop (Succ n) xs = xs??

Supose the list is empty 
 take (Succ n) [] ++ drop (Succ n) [] 
  = [] ++ drop (Succ n) []   by definition of take ->
  = [] ++ []                 by definition of drop ->
  = []                       by Exercise One

supose the list of not empty
 take (Succ n) (x:xs) ++ drop (Succ n) (x:xs) 
  = x : take n xs ++ drop (Succ n) (x:xs) 	by definition of take ->
  = (x : (take n xs)) ++ drop n xs   		by definition of drop ->
  = x : (take n xs ++ drop n xs)		by definition of ++
  = x : xs 					by inductive hypothesis

-}

--EX 6 

{-

Define the following functions over Nat

data Nat = Zero | Succ Nat deriving Show

even :: Nat -> Bool that determines whether a Nat represents an even number.

even Zero = True
even (Succ Zero) = False
even (Succ (Succ n)) = even n

double :: Nat -> Nat that doubles a given Nat

double Zero = Zero
double (Succ n) = (Succ (Succ (double n))

Prove that even (double n) = True for any n.

Base case, n is Zero
even (double Zero)
  = even Zero 	By definition of double
  = True 	By definition of even

inductive case, supose even (double n) = True

even (double (Succ n))
  = even (Succ (Succ (double n)) 	By definition of double
  = even (double n)			By definition of even
  = Ture 				By inductive hyposies


-}

--EX7

{-


Using the data type data Tree a = Leaf a | Node (Tree a) (Tree a) show that in any such tree the number of leaves is always one greater than the number of nodes.


data Tree a = Leaf a | Node (Tree a) (Tree a)

--leaves :: Tree a -> Int
--leaves (Leaf a) = 1
--leaves (Node l r) = leaves l + leaves r

--nodes :: Tree a -> Int
--nodes (Leaf a) = 0
--nodes (Node l r) = nodes l + nodes r + 1

-- Prove that leaves t = nodes t + 1  for any t


Base case Tree a contains a Leaf a as a direct child
leaves (Leaf a) = nodes(Leaf a) + 1

Inductive case, supose leaves l = nodes l + 1 and leaves r = nodes r + 1

leaves (Node l r)
  = leaves l + leaves r 	By definition of leaves
  = nodes l + 1 + nodes r + 1 	By inductive hypothesis
  = (nodes l + nodes r + 1) + 1	By rearanging
  = nodes (Node l r) + 1	By definition of nodes

-}


---EX8

{-

Verify the functor laws for the functors Maybe and Tree a where


instance Functor Maybe where
 -- fmap :: (a -> b) -> Maybe a -> Maybe b
 fmap g Nothing = Nothing
 fmap g (Just x) = Just (g x) 

instance Functor Tree where
 -- fmap :: (a -> b) -> Tree a -> Tree b
 fmap g (Leaf x) = Leaf (g x)
 fmap g (Node l r) = Node (fmap g l) (fmap g r)	

id :: a -> a
id (\x -> a)

Law 1 
fmap id x = id x

Maybe:
fmap id Nothing = Nothing
		= id Nothing

fmap id (Just x) = Just (id x)
		 = Just x
		 = id (Just x)

Tree:
Base Case 
fmap id (Leaf x) = Leaf (id x)
		 = Leaf x
		 = id (Leaf x)

Inductive Case, Supoes (fmap id l) = id l and fmap id r = id r
fmapp id (Node l r) = Node (fmap id l) (fmap id r)
		    = Node (id l) (id r)
		    = Ndoe l r
		    = id (Node l r)

Law 2:

fmap (g.h) = fmap g . fmap h

Maybe:

fmap (g.h) Nothing = Nothing 
		   = fmap g Nothing
		   = fmap g . fmap h Nothing

fmap (g.h) Just x = Just (g.h x)
		  = fmap g (Just (h x))
		  = fmap g . fmap h x

Tree:

Base case:

fmap (g.h) (Leaf x) = Leaf (g.h x)
		    = fmap g (Leaf (h x))
		    = fmap g . fmap h (Leaf x)


inductive case, supose (fmap (g.h) l) = fmap g . fmap h l And (fmap (g.h) r) = fmap g . fmap h r
fmap (g.h) (Node l r) = Node (fmap (g.h) l) (fmap (g.h) r)
		      = Node (fmap g . fmap h l) (fmap g . fmap h r)
		      = fmap g (Node ( fmap h l) ( fmap h r))
		      = fmap g . fmap h (Node l r)



-}


--EX9

{-

Identify the redexes in the following expressions and categorise them as innermost, outermost, neither or both

1 + (2 * 3)

- 1 + (Outter Most / Left Most)
- 2 * (inner most / Right Most)

(1 + 2) * (2 + 3)

- 1 + (Inner Most / Left Most)
- (1 + 2) * (Outter Most)
 - 2 + (Inner Most / Right Most)

fst (1 + 2 , 2 + 3)

- fst (Left Most, Outter Most)
- 1 + (IM)
- 1 + 2 , 
- 2 + (IM RM)

(\x -> 1 + x) ( 2 * 3 )

- (\x -> 1 + x) (OM LM)
- 2 * (IM RM)

-}

--EX10

{-

Define mult = \x -> (\y -> x * y) and show all of the evaluation steps that Haskell would take when evaluating mult (mult 3 4) 5


mult (mult 3 4) 5

Evaluation stragedy Call by Value, Inner Most Left Most

mult (mult 3 4) 5
 = (\x -> (\y -> x * y)) (mult 3 4) 5
 = (\x -> (\y -> (mult 3 4) * 5))
 = (mult 3 4) * 5
 = ((\x -> (\y -> x * y)) 3 4) * 5
 = (\x -> (\y -> 3 * 4))  * 5
 = (3 * 4) * 5
 = 12 * 5
 = 60

-}

--EX 11

{-

Use a list comprehension to define the infinite Fibonacci sequence 0,1,1,2,3,5,8,13,21,34 ... You will find it useful to use zip and tail and you may want to use the type Integer rather than Int.

-}

fibs :: [Integer]

fibs = 0 : 1 : [ n + m  | (n,m) <- zip fibs (tail fibs)  ]



--EX 12 

{-


Recall the functions repeat :: a -> [a] and take :: Int -> [a] -> [a] for lists? They produce an infinite list of copies of the given element and a prefix of a given size of a list respectively. We can compose them to define replicate n a that produces a fixed size list of copies of a.

Write analagous functions

data Tree a = Leaf a | Node (Tree a) (Tree a)

repeatTree :: a -> Tree a
takeTree :: Int -> Tree a -> Tree a and
replicateTree :: Int -> a -> Tree a
that work on the Tree data type. Make sure you define the latter function modularly.

-}

data Tree a = Leaf a | Node (Main.Tree a) (Main.Tree a) deriving (Eq, Show)

repeatTree :: a -> Main.Tree a

repeatTree x = Main.Node (repeatTree x) (repeatTree x)


nextLeaf :: Main.Tree a -> Main.Tree a

nextLeaf (Leaf a) = Leaf a
nextLeaf (Main.Node l r) = nextLeaf l

takeTree :: Int -> Main.Tree a -> Main.Tree a 

takeTree 0 _ = error "No Tree"
takeTree n (Leaf a) = (Leaf a)
takeTree 1 (Main.Node l r) = nextLeaf l
takeTree n (Main.Node l r) = Main.Node (takeTree (n-1) l) (takeTree (n-1) r)



replicateTree :: Int -> a -> Main.Tree a

replicateTree n = (takeTree n) . repeatTree














































