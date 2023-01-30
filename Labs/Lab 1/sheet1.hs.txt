--COMP2209 Autumn 2019
--Sample solutions to Exercise Sheet 1

-- Exercise One
-- double (double 2)
-- = double 2 + double 2
-- = 2 + 2 + double 2
-- = 2 + 2 + 2 + 2 
-- = 4 + 2 + 2
-- = 6 + 2
-- = 8
-- Yes, there are many ways of doing this. Resolving the innermost double 2 
-- would be more efficient than the above

-- Exercise Two
-- sum [ x ] 
-- = sun (x:[])
-- = x + sum []
-- = x + 0 
-- = x

-- Exercise Three

product :: [Int] -> Int 
product [] = 1
product (x : xs) = x * product xs

-- Exercise Four

quicktros :: [Int] -> [Int]
quicktros [] = []
quicktros (x : xs) = quicktros rs ++ [x] ++ quicktros ls
    where  ls = [ a | a <- xs , a <= x ]
           rs = [ a | a <- xs , a > x ]

-- Exercise Five
quicksort' [] = []
quicksort' (x:xs) = quicksort' ls ++ [x] ++ quicksort' rs
                   where 
                     ls = [ a | a <- xs , a < x ]
                     rs = [ a | a <- xs , a > x ]

-- The effect of using < rather than <= in ls is that duplicate entries in the list would
-- be removed as part of the sorting process.

-- Exercise Six
(2^3)*4
(2*3)+(4*5)
2 + (3*(4^5))
(2^2)+(2^2)

-- Exercise Seven
-- Can't use uppercase N as a variable name
-- Need backquotes for div
-- Breaks the layout rule in the where clause - columns must be aligned
n = a `div` length xs
    where 
      a = 10
      xs = [1,2,3,4,5]

-- Exercise Eight

--['a','b','c'] :: [Char]

--('a','b','c') :: (Char, Char, Char)

-- ['a',3,True] is not well-typed. Must have single type of elements in lists.

-- ('a',3,True) :: Num b => (Char, b, Bool).  Writing Int instead of Num b => etc is okay.

-- [ (False, '0'), (True,'1')] :: [(Bool, Char)]

-- ( [True,False] , ['0','1'] ) :: ([Bool], [Char])

-- [tail, init, reverse] :: [[a] -> [a]]

-- [] :: [a]

-- 2 : 3 : [] : 4 : 5 : [] :: Num [a] => [[a]].  
-- Looks like it should be ill-typed. But Num [a] will never hold so it practically is.

-- [] : [] :: [[a]]

-- Exercise Nine

bools :: [Bool]
bools = [True,False]

nums :: [[Int]]
nums = [[1,2] , [4], []]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f x = f x

explode :: String -> [Char]
explode x = x
-- A String is already a [Char]


-- Exercise Ten

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f ( f x )

-- Exercise Eleven

-- Function types are not structural in the same way that Lists and Tuples
-- are.  The constructor for function types is lambda (\x -> e). To compare
-- two such terms for equality we would need to compare the bodies of functions.
-- This could be done syntactically but this would be of limited use. For example
--  add x y = x + y  and add' x y = y + x  would be considered to be unequal.
-- A more natural notion of equality (extensional equality) is to say two functions 
-- are equal if they produce equal outputs on the same inputs. Unfortunately this 
-- notion of equality is not decidable for Haskell, or indeed any sufficiently powerful language.
-- For a flavour of this, suppose we compare two Bool -> Bool functions, f and g.  We would need to
-- evaluate f True and g True and compare the output. What if g goes in to a non-terminating loop?
-- When do we stop it? 



