-- Modified from Hangman example from chapter 10 of Programming in Haskell, Graham Hutton, Cambridge University Press, 2016.
-- Author : Julian Rathke, 2018

import System.IO
--import System.Random

main = hangman (2*)

hangman :: (Int -> Int) -> IO ()
hangman f = do word <- chooseWord
               let ds = replicate (length word) '-'
               putStrLn ds  
               putStrLn "Try to guess it:"
               play word ds (f $ length ds)

secretGetLine :: IO String
secretGetLine = do hSetEcho stdin False
                   xs <- getLine
                   hSetEcho stdin True
                   return xs

putUpdate :: String -> IO String
putUpdate s = do putStr "Your answer so far is : "
                 putStrLn s
                 return s 

play :: String -> String -> Int -> IO ()
play word answerSoFar n | n <= 0 = putStrLn "You have used up all of your guesses, game over." 
                        | answerSoFar == word = putStrLn "Correct!!"
                        | otherwise = 
                          do putStrLn ("You have " ++ (show n) ++ " guesses remaining. Enter a character :  ")
                             guess <- getChar
                             _ <- getChar
                             putStrLn ""
                             updatedAnswer <- putUpdate (updateMatch word answerSoFar guess)
                             play word updatedAnswer (n-1)

updateMatch :: String -> String -> Char -> String
updateMatch [] [] c = []
updateMatch (x:xs) (y:ys) c | x==y = x : updateMatch xs ys c
updateMatch (x:xs) (y:ys) c | x==c = x : updateMatch xs ys c
updateMatch (x:xs) (y:ys) c | otherwise = '-' : updateMatch xs ys c

 
chooseWord :: IO String
chooseWord = do handle <- openFile "Words.txt" ReadMode 
                numberOfWords <- hGetLine handle
                r <- randomRIO (0::Int,read (numberOfWords)-1)
                skipLines r handle
                word <- hGetLine handle
                putStrLn ("Secret Word Chosen.")
                return word

skipLines :: Int -> Handle -> IO ()
skipLines 0 h = return ()
skipLines n h = do hGetLine h 
                   skipLines (n-1) h

