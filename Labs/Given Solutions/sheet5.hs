--COMP2209 Autumn 2017
--Sample solutions to Exercise Sheet 5

-- Exercise One
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) =  x==y 
occurs x (Node l y r) = case (compare x y) of
                         EQ -> True
                         LT -> occurs x l 
                         GT -> occurs x r

-- Exercise Two
-- There are different versions of folds for Trees 
-- I'm going with the one that replaces the constructors
-- throughout the structure rather than as a Foldable
foldTree :: (a -> b) -> (b -> a -> b -> b) -> Tree a -> b
foldTree leaff nodef (Leaf x) = leaff x 
foldTree leaff nodef (Node l x r) = 
     nodef (foldTree leaff nodef l) x (foldTree leaff nodef r) 

flatten = foldTree (\x -> [x]) (\ls x rs -> ls ++ (x:rs)) 

-- Exercise Three
data Expr = Val Int | Add Expr Expr | Sub Expr Expr deriving Show

foldExpr :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b 
foldExpr valf addf subf (Val n) = valf n 
foldExpr valf addf subf (Add e1 e2) = 
      addf (foldExpr valf addf subf e1 ) (foldExpr valf addf subf e2)
foldExpr valf addf subf (Sub e1 e2) = 
      subf (foldExpr valf addf subf e1 ) (foldExpr valf addf subf e2)


eval :: Expr -> Int
eval = foldExpr (\n -> n) (\x y -> x + y) (\x y -> x - y)

size :: Expr -> Int
size = foldExpr (\n -> 1) (\x y -> x + y + 1) (\x y -> x + y + 1)

-- Exercise Four

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop
   deriving Show

data Form = Negative | Positive | Unknown | Mixed | Either deriving Show

getForm :: Prop -> Form
getForm (Const _) = Unknown 
getForm (Var _) = Positive
getForm (Not p) = negateForm $ getForm p
getForm (And p q) = andForm (getForm p) (getForm q)
getForm (Imply p q) = andForm (negateForm $ getForm p) (getForm q)

negateForm Negative = Positive
negateForm Positive = Negative
negateForm Mixed = Mixed
negateForm Either  = Either  

andForm Positive Positive = Positive
andForm Negative Negative = Negative
andForm f Either = f
andForm Either f = f 
andForm _ _ = Mixed

--Exercise Five
data Pair a b = P (a,b) 

instance Functor (Pair a) where
     fmap f (P(x,y)) = P(x,f y)

data Fun a b = F (a -> b)

instance Functor (Fun a) where
     --fmap :: (b -> c) -> (Fun a b) -> (Fun a c)
     fmap f (F g) = F (f . g)

-- data Fun a b = F (a -> b)
-- instance Functor (Fun b) where 
--     fmap :: (a -> c) -> Fun a b -> Fun c b 
--     fmap f (F g) = F ( \ x :: c  -> ??? ) 
--   Need to produce a value of type b, could do 
--	 this using g if we had a value of type a, but need an 'inverse' to f to do this

-- Exercise Six
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



