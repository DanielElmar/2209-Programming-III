import Data.Char

mylast (x:[]) = x
mylast (x:xs) = mylast xs


mylast2 (x:xs) | xs == [] = x
               | otherwise = mylast2 xs

mylast3 xs = head (reverse xs)

mylast4 (x:xs) | xs == [] = x
               | otherwise = mylast2 (tail (x:xs))



fourth :: [a] -> a
fourth (x:y:z:q:xs) = head((tail(z:q:xs)!!0):xs)
fourth1 (x:y:z:q:xs) = q
fourth2 xs = xs!!3
fourth3 xs = head (tail (tail (tail xs)))



safetail :: [a] -> [a]

safetail xs = if null xs == True
              then []
              else tail xs

safetail2 xs | null xs == True = []
             | otherwise = tail xs

safetail3 [] = []
safetail3 (x:xs) = xs


halve2 :: [a] -> ([a],[a])

halve2 xs = (x,y)
      where
           x = take (div (length xs) 2) xs
           y = reverse (take (div (length xs) 2) (reverse xs))

halve3 xs | mod (length xs) 2 == 0 = halve2 xs
          | otherwise = error "Even lists only"




explode :: String -> [Char]
explode x = x 



nextChar :: Char -> Char
nextChar x = chr $ ord x + 1

encptString addint (s:[]) outputstring =  chr (ord s + addint) : outputstring 
encptString addint (s:string) outputstring =  encptString addint string (chr (ord s + addint) : outputstring)

enc :: Int -> String -> String
enc addint (s:string) = encptString addint (s:string) []


{-
encrypt :: Int -> String -> (String , String -> String)
encrypt int string = (x,y)
                   where x = enc int string
                         y = func s = encptString (-int) s []

-}


--Ex6

gradeToScore :: Char -> Int 
gradeToScore char | char == 'A' =  48
                  | char == 'B' =  40
                  | char == 'C' =  32
                  | char == 'D' =  24
                  | char == 'E' =  16
                  | otherwise = error "Not Grade Char"


addGradeToTotal :: String -> Int -> Int
addGradeToTotal (s:[]) runningTotal =  runningTotal + gradeToScore s
addGradeToTotal (s:string) runningTotal = addGradeToTotal string (runningTotal + (gradeToScore s))

meetsOffer :: String -> Int -> Bool
meetsOffer grades toMeet | addGradeToTotal grades 0 >= toMeet = True
                         | otherwise = False



--Ex7

luhnDouble :: Int -> Int

luhnDouble digit | (digit * 2) > 9 = (digit * 2) - 9
                 | otherwise = (digit * 2)


luhn :: Int -> Int -> Int -> Int -> Bool
luhn digit1 digit2 digit3 digit4 | ((luhnDouble digit1) + digit2 + (luhnDouble digit3) + digit4) `mod` 10 == 0 = True
                                 | otherwise = False




