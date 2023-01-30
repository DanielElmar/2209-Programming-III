--COMP2209 Autumn 2017
--Sample solutions to Exercise Sheet 6

-- Exercise Two
-- 
-- Reason by induction on List x
-- Base case, xs is []
-- [] ++ [] 
-- = []  by defintion  ->

-- Inductive case, suppose xs ++ [] = xs
-- (x:xs) ++ [] 
--   = x : (xs ++ [])    by definition ->
--   = x : xs  by the inductive hypothesis

-- Reason by induction on List x
-- Base case, xs is []
-- [] ++ (ys ++ zs) 
--   = ys ++ zs  by definition ->
--   =  ([] ++ ys) ++ zs  by definition <-

-- Inductive case, suppose xs ++ (ys ++ zs) = (xs + ys) ++zs
-- (x:xs) ++ (ys ++ zs) 
--  = x : (xs ++ (ys ++ zs))  by definition ->
--  = x : ((xs ++ ys) ++ zs)  by the inductive hypothesis
--  = (x : (xs ++ ys))  ++ zs by definition <-
--  = ((x:xs) ++ ys) ++ zs by definition <-

-- Exercise Three

-- Prove that all (== x) (replicate n x) = True for any x,n
-- Reason by induction on Nat n.
-- Base Case, n is Zero
-- all (== x) (replicate Zero x)
--  = all (== x) []  by definition of replicate ->
--  = True  by definition of all ->

-- Inductive case, suppose: all (== x) (replicate n x) = True
-- all (== x) (replicate (Succ n) x)
--  = all (== x) (x : replicate n x)  by definition ->
--  = (== x) x && all (== x) (replicate n x) by definition ->
--  = (== x) x && True  by the inductive hypothesis
--  =  x == x  rewriting as infix
--  =  True    by assumption on ==, yes, this is reasonable due to Eq.

-- Exercise Four
--
-- Prove that take n xs ++ drop n xs = xs  for any xs, n
-- Reason by induction on n
-- Base case n is Zero
-- take Zero xs ++ drop Zero xs 
--  = [] ++ drop Zero xs   by definition of take ->
--  = [] ++ xs             by definition of drop ->
--  = xs                   by Exercise One

-- Inductive case, suppose take n xs ++ drop n xs = xs
-- Suppose list is empty
-- take (Succ n) [] ++ drop (Succ n) [] 
--  = [] ++ drop (Succ n) []   by definition of take ->
--  = [] ++ []                 by definition of drop ->
--  = []                       by Exercise One
-- Suppose list is not empty.
-- take (Succ n) (x:xs) ++ drop (Succ n) (x:xs) 
--  = x : (take n xs) ++ drop (Succ n) (x:xs)  by definition of take ->
--  = (x : (take n xs)) ++ drop n xs           by definition of drop ->
--  = x : ( take n xs ++ drop n xs )           by definition of ++  ->
--  = x : xs                                   by the inductive hypothesis

-- Exercise Five

data Nat = Zero | Succ Nat deriving Show

even :: Nat -> Bool
even Zero = True
even (Succ n) = odd n

odd :: Nat -> Bool
odd Zero = False
odd (Succ n) = even n

double Zero = Zero 
double (Succ n) = Succ (Succ (double n))

-- Prove that even (double n) = True  for any n
-- Use induction on n

--  Base case
--    even (double Zero)
--  = even (add Zero Zero)
--  = even (Zero)
--  = True

--  Ind. Case 
--  even (double (Succ n))
--    = even (Succ (Succ (double n)))
--    = odd (Succ (double n))
--    = even (double n)
--    = True


--  Ind. Case
--    even (double (Succ n))
--  = even (add (Succ n) (Succ n))
--  = even (Succ (add n (Succ n)))
--  = not ( even (add n (Succ n)) )
--  = {lemma} not (even ( Succ (add n n) ) )
--  = not (not ( even (add n n) ) )
--  = not (not ( even (double n) ) )
--  = {IH} not (not ( True )
--  = not False
--  = True


--  Lemma :  add n (Succ m) = Succ ( add n m )
--  Proof by induction on n.

--  Base
--     add Zero (Succ m)
--  = Succ m
--  = Succ (add Zero m)

--  Ind Case
--     add (Succ n) (Succ m)
--   = Succ ( add n (Succ m) )
--   = {IH} Succ ( Succ ( add n m ) )
--   = Succ ( add (Succ n) m ) 


-- Exercise Six
--data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

--leaves :: Tree a -> Int
--leaves (Leaf a) = 1
--leaves (Node l r) = leaves l + leaves r

--nodes :: Tree a -> Int
--nodes (Leaf a) = 0
--nodes (Node l r) = nodes l + nodes r + 1

-- Prove that leaves t = nodes t + 1  for any t
-- 
-- Reason by Induction
-- Base case, t is Leaf a
-- leaves (Leaf a)
--  = 1 
--  = 0 + 1 
--  = nodes (Leaf a) + 1
-- 
-- Inductive case, suppose:  leaves l = nodes l + 1 and leaves r = nodes r + 1
-- leaves (Node l r) 
--   = leaves l + leaves r
--   = (nodes l + 1) + leaves r        by inductive hypothesis
--   = (nodes l + 1) + (nodes r + 1)   by inductive hypothesis
--   = (nodes l + nodes r + 1) + 1
--   = (nodes (Node l r) + 1)

-- Exercise Seven
--
-- Functor Law 1 for Maybe
-- fmap id Nothing = Nothing = id Nothing
-- fmap id (Just x) = Just (id x) = Just x = id (Just x)
-- Hence fmap id = id

-- Functor Law 2 for Maybe 
-- fmap (f . g) Nothing 
--  = Nothing 
--  = fmap f Nothing
--  = fmap f (fmap g Nothing)
--  = (fmap f . fmap g) Nothing
-- fmap (f . g) (Just x)
--   = Just ((f . g) x)
--   = Just (f (g x))
--   = fmap f (Just g x)
--   = fmap f (fmap g (Just x))
--   = (fmap f . fmap g ) (Just x)
--  Hence  fmap (f.g) = (fmap f).(fmap g)

--  Functor Law 1 for Tree
--  Reason by induction on the Tree
--  Base Case
--  fmap id (Leaf x) = Leaf (id x) = (Leaf x) = id (Leaf x)
--  Inductive Case, suppose (fmap id l) = id l and fmap id r = id r
--  fmap id (Node l r) 
--    = Node (fmap id l) (fmap id r)) 
--    = Node (id l) (id r)
--    = Node l r
--    = Id (Node l r)

--  Functor Law 2 for Tree
--  Reason by induction on the Tree
--  Base Case
--  fmap (f.g) (Leaf x) 
--   = Leaf (f.g x) 
--   = Leaf (f (g x))
--   = fmap f (Leaf (g x))
--   = fmap f (fmap g (Leaf x))
--   = (fmap f) . (fmap g) (Leaf x)
--  Inductive Case, suppose 
--      fmap (f.g) l = (fmap f) . (fmap g) l and
--      fmap (f.g) r = (fmap f) . (fmap g) r then
--   fmap (f.g) (Node l r)
--     = Node (fmap (f.g) l) (fmap (f.g) r)
--     = Node ((fmap f) . (fmap g) l ) ((fmap f).(fmap g) r)
--     = Node ((fmap f) (fmap g l))  ( (fmap f) (fmap g r) )
--     = fmap f (Node (fmap g l) (fmap g r))
--     = fmap f ( fmap g ( Node l r ))
--     = (fmap f).(fmap g) (Node l r) 
--  Hence fmap f.g = fmap f . fmap g 


-- Exercise Eight

--  -- 1 + ( 2 * 3)
-- redexes :  2 * 3  inner/outermost

-- (1 + 2) * ( 2 + 3) 
-- redexes :  1 + 2  and 2 + 3 both inner/outermost

-- fst (1 + 2, 2 + 3)
-- redexes :  fst ( n , _) outer and 1 + 2 inner and 2 + 3 inner

-- ( \x -> 1 + x) (2 * 3)
-- redexes  ( \x . ..) (n)  outer  and 2 * 3  inner


-- Exercise Nine

mult = \x -> ( \y -> x * y)

-- mult (mult 3 4) 5 
-- --> (\x -> ( \y -> x * y)) (mult 3 4) 5
-- --> ( \y -> (mult 3 4) * y) 5
-- --> (mult 3 4) * 5
-- --> ((\x -> (\y -> x * y)) 3 4) * 5
-- --> ((\y -> 3 * y) 4) * 5
-- --> (3*4)*5
-- --> 12 * 5
-- --> 60

-- Exercise Ten

fibs :: [Integer]
fibs = 0 : 1 : [ n + m | (n,m) <- zip fibs (tail fibs) ]

-- Exercise Eleven 

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeatTree :: a -> Tree a
repeatTree a = Node (repeatTree a) a (repeatTree a)

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _ = Leaf
takeTree _ Leaf = Leaf
takeTree n (Node l a r) = Node (takeTree (n-1) l) a (takeTree (n-1) r)

replicateTree n = (takeTree n) . repeatTree
