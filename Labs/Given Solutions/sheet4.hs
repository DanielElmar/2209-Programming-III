--COMP2209 Autumn 2017
--Sample solutions to Exercise Sheet 4

import Data.List

-- Exercise One
all :: (a -> Bool) -> [a] ->Bool
all p = foldr (&&) True . map p

any :: (a -> Bool) -> [a] ->Bool
any p = foldr (||) False . map p

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (consp) []
    where consp x xs = if p x then x : xs else []

-- The next exercise is quite tricky. The solution below uses higher-order functions.
-- The foldr builds up a function! This is a composition of tail functions for as long
-- as the predicate holds. Once the predicate fails, the composition of tail functions
-- is composed with the identity function.
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = (foldr (dropFuns) id xs) xs 
   where dropFuns x fs = if p x then fs . tail else id 

-- Exercise Two
dec2int :: [Int] -> Int
dec2int = foldl (\n m -> n * 10 + m ) 0 

-- Exercise Three
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \a b -> f(a,b)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y

-- Exercise Four
type Bit = Int

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin  = unfold (== 0) (`mod` 2) (`div` 2)

chop :: Int -> String -> [ String ]
chop n = unfold (null) (take n) (drop n)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold (null) (f.head) (tail)

iterate :: (a -> a) -> a -> [a] 
iterate = unfold (\_ -> False) (id)

-- Exercise Five
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (a:as) = f a : altMap g f as

-- Exercise Six 
luhn :: [Int] -> Bool
luhn =  (divby10) . sum . altMap (\x -> x) (luhnDouble)  . reverse
        where luhnDouble n = if m > 9 then m-9 else m 
                 where m = n*2
              divby10 n = n `mod` 10 == 0

-- Exercise Seven
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

halve :: [a] -> ([a],a,[a])
halve xs = let n = (length xs) `div` 2 in (take n xs , xs!!n , drop (n+1) xs) 

balance :: [a] -> Tree a
balance [] = Leaf
balance xs  =  Node (balance ls) x (balance rs)
                where (ls,x,rs) = halve xs

toTree :: Ord a => [a] -> Tree a
toTree = balance . sort

-- To test if this is correct
heights :: Tree a -> [Int]
heights Leaf = [0]
heights (Node l x r) = map (1+) (heights l ++ heights r)

isBalanced :: Tree a -> Bool
isBalanced t = Main.all (<2) [ h1-h2 | h1 <- heights t , h2 <- tail $ heights t]
                
-- Exercise Eight
data Nat = Zero | Succ Nat deriving (Eq,Ord,Show,Read)

evenN :: Nat -> Bool
evenN Zero = True
evenN (Succ n) = oddN n

oddN :: Nat -> Bool
oddN Zero = False
oddN (Succ n) = evenN n

addN :: Nat -> Nat -> Nat
addN Zero m = m
addN (Succ n) m = Succ (addN n m)

multN :: Nat -> Nat -> Nat
multN Zero m = Zero
multN (Succ n) m = addN (multN n m) m


-- Exercise Nine
data RInt = RZero | RSucc RInt | RPred RInt

isAllPred :: RInt -> Bool
isAllPred RZero = True
isAllPred (RSucc n) = False
isAllPred (RPred n) = isAllPred n

isAllSucc :: RInt -> Bool
isAllSucc RZero = True
isAllSucc (RPred n) = False
isAllSucc (RSucc n) = isAllSucc n

normalise :: RInt -> RInt 
normalise RZero = RZero
normalise (RSucc n) | isAllSucc n = RSucc n
                   | otherwise = normalise (removePred n)
        where removePred (RPred n) = n
              removePred (RSucc n) = RSucc (removePred n)
normalise (RPred n) | isAllPred n = RPred n
                   | otherwise = normalise (removeSucc n)
        where removeSucc (RSucc n) = n
              removeSucc (RPred n) = RPred (removeSucc n)



oddRI :: RInt -> Bool
oddRI RZero = False
oddRI (RSucc n) = evenRI n
oddRI (RPred n) = evenRI n

evenRI :: RInt -> Bool
evenRI RZero = True
evenRI (RSucc n) = oddRI n
evenRI (RPred n) = oddRI n

addRI :: RInt -> RInt -> RInt 
addRI RZero m = m
addRI (RSucc n) m = RSucc ( addRI n m ) 
addRI (RPred n) m = RPred ( addRI n m )

negateRI :: RInt -> RInt 
negateRI RZero = RZero
negateRI (RSucc n) = RPred (negateRI n)
negateRI (RPred n) = RSucc (negateRI n)

multRI :: RInt -> RInt -> RInt
multRI n m = multN (normalise n) (normalise m)
  where multN :: RInt -> RInt -> RInt 
        multN RZero n = RZero
        multN (RSucc n) m = addRI (multN n m) m
        multN (RPred n) m = negateRI (multN (negateRI (RPred n)) m)




