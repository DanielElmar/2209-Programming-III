import Data.Char
import Data.List

-- EX1

--Decide if all elements of a list satisfy a predicate
all :: (a -> Bool) -> [a] -> Bool

all f xs | length xs == length (filter f xs) = True
         | otherwise = False


--Decide if any element of a list satisfies a predicate
any :: (a -> Bool) -> [a] -> Bool

any f xs | ( length (filter f xs) ) > 0 = True
         | otherwise = False

--Select the initial elements from a list while they satisfy a predicate
takeWhile :: (a -> Bool) -> [a] -> [a]

takeWhile p ps = foldr (consp) [] ps
    where consp x xs = if p x then x : xs else []


--Remove the initial elements from a list while they satisfy a predicate
dropWhile :: (a -> Bool) -> [a] -> [a]

dropWhile p xs = (foldr (dropFuns) id xs) xs 
   where dropFuns x fs = if p x then fs . tail else id 


--EX2

decInt :: [Int] -> Int


decInt = foldl (\acc lastElem-> acc * 10 + lastElem) 0 

--EX 3

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \a b -> f(a,b)

unCurry :: (a -> b -> c) -> ((a,b) -> c)
unCurry f = \(a,b) -> f a b

--EX4

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)


type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold ( == 0) (`mod` 2) (`div` 2) 


chop :: Int -> [Char] -> [[Char]] 
chop n = unfold (null) (take n) (drop n)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold (null) (f.head) (drop 1)


iterate :: (a -> a) -> a -> [a]
iterate f x = unfold (\_ -> False) (id) f x

--EX 5

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]


{-altMap f p [] = []
altMap f p (x:xs) | length xs `mod` 2 == 0 = f x : altMap f p xs
                  | otherwise = p x : altMap f p xs-}

altMap f g [] = []
altMap f g (a:as) = f a : altMap g f as

--EX 6



luhn :: [Int] -> Bool
luhn xs =  ((divby10) . sum . altMap (\x -> x) (luhnDouble) . reverse )xs
        where luhnDouble n = if m > 9 then m-9 else m 
                 where m = n*2
              divby10 n = n `mod` 10 == 0


luhn2 :: [Int] -> Bool
luhn2 = divby10 . sum . altMap (luhnDouble) (id) 
          where
               luhnDouble n = 2 * n `mod` 9
               divby10 n = n `mod` 10 == 0


--EX 7

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

--toTree :: Ord => [a] -> Tree a

halve :: [a] -> ([a],a,[a])
halve xs = ( take n xs , xs!!n , drop (n+1) xs )
          where n = length xs `div` 2 

toTree [] = Leaf
toTree (x:[]) = Node Leaf x Leaf
toTree xs = Node (toTree lt) v (toTree rt)
           where (lt,v,rt)= halve (sort xs)

--EX 8 

data Nat = Zero | Succ Nat deriving (Eq,Ord,Show,Read)


myEven :: Nat -> Bool

myEven Zero = True
myEven (Succ n) = myOdd n


myOdd :: Nat -> Bool

myOdd Zero = False
myOdd (Succ n) = myEven n


add :: Nat -> Nat -> Nat

add Zero m = m

add (Succ n) m = Succ ( add n m ) 



mult :: Nat -> Nat -> Nat

mult Zero m = Zero
mult (Succ n) m = add m (mult n m)


--EX 9 

data RInt = RZero | RSucc RInt | RPred RInt deriving Show

{-
normalise Zero2 = Zero2
normalise (Succ2 ( Pred2 n)) = normalise  n
normalise (Pred2 ( Succ2 n)) = normalise  n
normalise (Succ2 n) = Succ2 $ normalise n
normalise (Pred2 n) = Succ2 $ normalise n
-}

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
             where
                  multN RZero m = RZero
                  multN (RSucc n) m = addRI m (multN n m)
                  multN (RPred n) m = negateRI ( multN ( negateRI (RPred n) ) m ) 




















































