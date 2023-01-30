double x = x + x
mysum [] = 0
mysum (x:xs) = x + mysum xs

myproduct [x] = x
myproduct [] = 1
myproduct (x:xs) = x * myproduct xs


quicktros [] = []
quicktros (x:xs) = quicktros ls ++ [x] ++ quicktros rs
                   where 
                     ls = [ a | a <- xs , a >= x ]
                     rs = [ a | a <- xs , a < x ]

n = a `div` length xs
           where 
               a = 10 
               xs = [1,2,3,4,5]

add a b c = (a::Int) + (b::Int) + (c::Int)

copy a = (a,a)

apply f a = f a 

explodes a = list
           where
                list = [ b | b <- a ]

explode :: String -> [Char]
explode x = x 


