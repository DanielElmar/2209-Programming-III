import Data.Char

-- EX1

ex1 = sum ([x^2 | x <- [1..100], odd x] ++ [x^3 | x <- [1..100], even x])


--EX2

grid :: Int -> Int -> [(Int,Int)]

grid m n = [ (x,y) | x <-[0..m], y <-[0..n] ]

square :: Int -> [(Int,Int)]

square  n = [ (x,y) | x <-[0..n], y <-[0..n], x/=y ]


--EX3

replicate :: Int -> a -> [a]

replicate n a = [ a | x<-[0..n] ]

--EX4 

pyths :: Int -> [(Int,Int,Int)]

pyths n = [ (x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], ((x^2 + y^2) == z^2) ]


--EX5 


perfect :: Int -> [Int]

perfect n = [ per | per<-[1..n] , sum [ fac | fac<-[1..(per-1)], per `mod` fac == 0 ] == per ]


--EX6

find :: Eq a => a -> [ (a,b)] -> [b]
find k t = [ v | (k',v) <- t, k==k']


positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])


--EX7

scalarP :: [Int] -> [Int] -> Int

scalarP xs ys = sum [ a*b | (a,b)<-zip xs ys]


--EX8

shorter :: [a] -> [a] -> Bool

shorter [_] [] = True
shorter [] [_] = False
shorter [] [] = error "List are the same length"
shorter xs ys = shorter (tail xs) (tail ys)


--EX9


euclid :: Int -> Int -> Int

euclid a b | a == b = a
           | a > b = euclid (a-b) b
           | otherwise = euclid a (b-a)


--EX10


minHamming :: Eq a => [[a]] -> Int

{-
minHamming strings = [ (strings!!si1, strings!!si2, (sum [ 1 | index<-[0..((length (strings!!si1)) -1)], (strings!!si1)!!index /= (strings!!si2)!!index]))  | si1<-[0..((length strings) -1)],  si2<-[0..((length strings) -1)], si1/=si2 ]

-}
--   (sum [ 1 | index<-[0..((length strings!!si1) -1)], strings!!si1!!index /= strings!!si2!!index])  

minHamming strings = minimum[ (sum [ 1 | index<-[0..((length (strings!!si1)) -1)], (strings!!si1)!!index /= (strings!!si2)!!index])  | si1<-[0..((length strings) -1)],  si2<-[0..((length strings) -1)], si1/=si2 ]


hamming :: Eq a => [a] -> [a] -> Int
hamming s1 s2 = sum [ 1 | index<-[0..(length s1 -1)], s1!!index /= s2!!index ]


--EX11

{-
merge :: Ord a => [a] -> [a] -> [a] -> [a]

merge l1 [] ol = ol ++ l1
merge [] l2 ol = ol ++ l2
merge l1 l2 ol | head l1 <= head l2 = merge (tail l1) l2 ((head l1) : ol)
               | otherwise = merge (l1) (tail l2) ((head l2) : ol)
-}

merge :: Ord a => [a] -> [a] -> [a]

merge l1 [] = l1
merge [] l2 = l2

{-
merge l1 l2 | head l1 <= head l2 = head l1 : merge (tail l1) l2
            | otherwise = head l2 : merge l1 (tail l2) 
-}

merge l1@(x:xs) l2@(y:ys) | x <= y = x : merge xs l2
            | otherwise = y : merge l1 ys 


halve ::[a] -> ([a],[a])

--halve l = ( [ l!!i | i<-[0..((length l - 1) `div` 2)] ] , [ l!!i | i<-[((length l + 1) `div` 2)..(length l -1)]] )

halve l = ( [ l!!i | i<-[0..((n - 1) `div` 2)] ] , [ l!!i | i<-[((n + 1) `div` 2)..(n -1)]] )
          where
          n = length l


mergeSort :: Ord a => [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge l1 l2
              where
              n = (length l) `div` 2
              fh = take n l
              sh = drop n l
              l1 = mergeSort fh
              l2 = mergeSort sh



--EX A1



vigenere :: String -> (String -> String, String -> String)


--vigenere  s = (\x -> x, \y -> y)


{-
vigenere k = (  \s -> [ key!!x | i<-[0..(length s -1)], x<-[i `mod` length key]   ] , \s -> s  )
              where
              upperK = map toUpper k
              key = [ upperK!!i | i<-[0..(length k -1)], ord (upperK!!i) >= 65 && ord (upperK!!i) <= 90]

-}

-- s = [ map toUpper s!!i | i<-[0..(length s -1)], ord (map toUpper s!!i) >= 65 && ord (map toUpper s!!i) <= 90]

-- filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper "dsd 9()()34nf dfd")



vigenere k = (  \s -> zipWith (\x y -> chr( 65 + mod (ord x + ord y - 130) 26 )) (filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper s)) [ key!!x | i<-[0..(length s -1)], x<-[i `mod` length key] ] , \es -> zipWith (\x y -> chr( 65 + mod (ord x - ord y - 130) 26 )) (filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper es)) [ key!!x | i<-[0..(length es -1)], x<-[i `mod` length key] ]  )
            where
            key = filter (\x -> ord x >= 65 && ord x <= 90 ) (map toUpper k)
              



--EX A2



count :: String -> [(Char, Int)]
count s = [ (c,length $ filter (== c) (map toUpper s)) | c <- ['A'..'Z'], (length $ filter (== c) (map toUpper s)) /= 0 ]


frequency :: Int -> String -> [[(Char, Int)]]



frequency n ct = [  count s   | s <-[ [ct!!(offset + (offsetMultiple * n))  |offsetMultiple <- [0..quot(length ct - offset - 1) n ] ] | offset <- [0..n-1]  ]]

























