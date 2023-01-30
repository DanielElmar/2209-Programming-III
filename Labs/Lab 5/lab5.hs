import Data.Char
import Data.List

-- EX1



data Tree a = Leaf a | Node (Tree a) a (Tree a) 

occurs :: Ord a => a -> Tree a -> Bool


occurs n (Leaf a) = compare n a == EQ
occurs n (Node lt v rt) | compare v n == EQ = True
                        | compare n v == LT = occurs n (lt)
                        | otherwise = occurs n (rt)




--EX 2


-- data Tree a = Leaf a | Node (Tree a) a (Tree a)	

foldTree :: (a -> b -> b) -> b -> Tree a -> b

foldTree f base (Leaf a) = f a base
foldTree f base (Node lt a rt) = foldTree f base' lt
                              where
                              base' = f a base''
                              base'' = foldTree f base rt
flatten :: Tree a -> [a]

flatten a = foldTree (:) [] a


--EX3

data Expr = Val Int | Add Expr Expr | Sub Expr Expr


--foldExpr :: ( a -> Int -> Int) -> Int -> Expr -> Int
--foldExpr :: ( Int -> b -> b) -> b -> Expr -> b

foldExpr :: ( Int -> Int -> Int ) -> Int  -> Expr -> Int 

foldExpr f v (Val a) = f a v
foldExpr f v (Add l r) = foldExpr f (foldExpr f v r) l
foldExpr f v (Sub l r) = foldExpr f ( - foldExpr f v r) l


eval :: Expr -> Int
eval a = foldExpr (+) 0 a

size :: Expr -> Int
size a = foldExpr (\x y -> y + 1) 0 a


--EX 4

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop deriving (Eq, Show)

data Form = Positive | Unknown | Negative | Mixed deriving (Eq, Show)

negateProp :: Form -> Form
negateProp Positive = Negative
negateProp Negative = Positive
negateProp Mixed = Mixed
negateProp Either = Either

mergeForm :: Form -> Form -> Form
mergeForm Positive Positive = Positive
mergeForm Negative Negative = Negative
mergeForm Mixed _ = Mixed
mergeForm _ Mixed = Mixed
mergeForm Positive _ = Mixed
mergeForm Negative _ = Mixed
mergeForm f Either = f
mergeForm Either f = f 




getForm :: Prop -> Form 
           
getForm (Const _) = Unknown
getForm (Var a) = Positive
getForm (Not a) = negateProp $ getForm a
getForm (And a b) = mergeForm (getForm a) (getForm b)
getForm (Imply a b) = mergeForm (negateProp $ getForm a ) (getForm b)



-- EX 5 

data Pair a b = P (a, b) deriving (Eq, Show)


-- (c -> b) -> Pair a c -> Pair a b
-- ((a1, b1) -> (a2, b2)) -> Pair a1 b1 -> Pair a2 b2
--   \(x,y)->(x+1,y+1)       P(4,5)

fir (P(a,b)) = a
sec (P(a,b)) = b

instance Functor (Pair a) where


fmap1 :: (t -> b) -> Pair a t -> Pair a b
fmap1 f (P (a,b)) = P ( a , f b) 

{-
fmap1 (b -> c) (P (a,b)) = P ( a , f b) 
fmap1 (a -> c) (P (a,b)) = P (f a , b) 
-}


data Fun c d = F (c -> d)

instance Functor ( Fun c ) where

-- Functor f => (a(b -> b(e) -> f a(d -> f b(e

fmap2 f (F (e)) = F (f.(e))

--EX 6


data LTree a = LLeaf a | LNode (LTree a) (LTree a) deriving Show
data Direction a = L (LTree a) | R (LTree a)
type Trail a = [Direction a]
type Zipper a = (LTree a, Trail a)

x -: f = f x
goLeft  (LNode l r, ts) = (l, L r : ts)
goLeft _ = error "Invalid path in tree"
goRight (LNode l r, ts) = (r, R l : ts)
goRight _ = error "Invalid path in tree"
goUp    (t , L r : ts) = (LNode t r , ts)
goUp    (t , R l : ts) = (LNode l t , ts) 
goUp _ = error "Invalid path in tree"

increment :: Enum a => Zipper a -> Zipper a
increment ((LLeaf x), ts) = ((LLeaf (succ x)), ts)

goLeftMost :: Zipper a -> Zipper a
goLeftMost (LLeaf a, ts) = (LLeaf a, ts)
goLeftMost z@((LNode l r), ts) = goLeftMost $ goLeft z

goRightMost :: Zipper a -> Zipper a
goRightMost (LLeaf a, ts) = (LLeaf a, ts)
goRightMost z@((LNode l r), ts) = goRightMost $ goRight z

goRoot :: Zipper a -> Zipper a
goRoot (t,[]) = (t,[])
goRoot z = goRoot (goUp z)


incr2LR :: Enum a => LTree a -> LTree a
incr2LR (LLeaf a) = (LLeaf a)
incr2LR tree = 
 fst $ (tree,[]) -: goLeftMost -: goUp -: goRight -: goLeftMost -: increment -: goRoot 
                 -: goRightMost -: goUp -: goLeft -: goRightMost -: increment -: goRoot

























