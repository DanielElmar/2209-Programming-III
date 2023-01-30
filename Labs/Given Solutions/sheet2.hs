--COMP2209 Autumn 2019
--Sample solutions to Exercise Sheet 2
--Non-assessed exercises

import Data.Char

-- Exercise One

last' :: [Int] -> Int
last' xs = xs !! n
   where n = (length xs -1)

last'' :: [Int] -> Int
last'' xs = head ( drop n xs )
   where n = (length xs -1)

-- Exercise Two
fourth :: [a] -> a
fourth n = head (tail (tail (tail n)))

fourth' :: [a] -> a
fourth' = (!!3)

fourth'' :: [a] -> a
fourth'' (_ : _ : _ : n : _ ) = n

-- Exercise Three
safetail :: [a] -> [a]
safetail ns = if null ns then [] else tail ns

safetail' :: [a] -> [a]
safetail' ns | null ns = []
             | otherwise = tail ns

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (n:ns) = ns
-- The pattern matching solution is perhaps most concise.

-- Exercise Four
halve :: [a] -> ([a], [a])
halve xs = (take n xs , drop n xs) 
 where n = length xs `div` 2

-- It is not so easy to define this using pattern matching as we need to pattern match
-- n/2 elements of the list where n changes dynamically with the length of the list.

-- Exercise Five
enc :: Int -> String -> String
enc n [] = []
enc n (c:cs) = chr ( (ord c) + n ) : enc n cs

encrypt :: Int -> String -> (String, String -> String)
encrypt n s = (enc n s, dec)
  where --dec [] = []
        --dec (c:cs) = chr ((ord c) - n) : dec cs
        dec = enc (-n)

-- Exercise Six
-- I've chosed error handling to simply skip any Chars that are not A Level grades
-- i.e. make a best effort sum
-- A reasonable alternative is to throw an error
meetsOffer :: String -> Int -> Bool
meetsOffer grades offer = sumGrades grades >= offer
   where sumGrades [] = 0
         sumGrades ('A':'*':grades) = 56 + sumGrades grades
         sumGrades (g:grades) | 'A' <= g && g <= 'E' = ((ord 'E' - ord g)+1)*8  + sumGrades grades
                              | otherwise = sumGrades grades 
                              -- | otheriwse = error "Input contains characters that don't represent A-level grades."

-- Exercise Seven

luhnDouble :: Int -> Int
luhnDouble n | m > 9 = m-9
             | otherwise = m 
  where m = n*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn n1 n2 n3 n4 = 
     (luhnDouble n1 + n2 + luhnDouble n3 + n4) `mod` 10 == 0
