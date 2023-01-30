
import Control.Monad
import Data.List
import System.IO
--import System.Random
import Parsing

--EX 1

sequenceIO_ :: [IO a] -> IO ()

sequenceIO_ [] = do
                    return ()
sequenceIO_ (a:as) = do
                        a
                        sequenceIO_ as
                        return ()

putStrLn :: String -> IO ()
putStrLn string = do
                      sequenceIO_ [ (putChar c) | c <- string]
                      putChar '\n'
                      return ()


-- EX 2
addNumbers :: Int -> Int -> IO Int

addNumbers 0 acc =  return acc
addNumbers remaining acc = do
                              x <- getLine
                              addNumbers (remaining - 1) (acc + read x)


adder :: IO ()
adder = do
          putStr "How many numbers? "
          x <- getLine
          y <- addNumbers (read x) 0
          Main.putStrLn ("The total is :" ++ show y ++ "\n")
          return ()


sequenceIO :: [ IO a ] -> IO [ a ]

sequenceIO [] = return []
sequenceIO (a:as) = do
                        x <- a
                        xs <- sequenceIO as
                        return (x : xs)

adder2 :: IO ()
adder2 = do
           putStr "How many numbers? "
           x <- getLine
           y <- sequenceIO [ getLine | _ <- [1..(read x)] ]
           let tot = sum $ map read y
           Main.putStrLn ("The total is :" ++ show tot ++ "\n")
           return ()


--EX 4



-- given n, "queens n" solves the n-queens problem, returning a list of all the
-- safe arrangements. each solution is a list of the columns where the queens are
-- located for each row
queens :: Int -> [[Int]]
queens n = map fst $ foldM oneMoreQueen ([],[1..n]) [1..n]  where
  oneMoreQueen (y,d) _ = [(x:y, delete x d) | x <- d, safe x]  where
    safe x = and [x /= c + n && x /= c - n | (n,c) <- zip [1..] y]

-- prints what the board looks like for a solution; with an extra newline
printSolution :: [ Int ] -> IO ()
printSolution y = do
     let n = length y
     let bars = replicate (4*n+1) '-'
     mapM_ (\x -> Main.putStrLn (bars ++ ['\n'] ++ ('|': concatMap (\c -> [' ', c ,' ', '|']) [if z == x then 'Q' else '.' | z <- [1..n]]))) y
     Main.putStrLn bars
     Main.putStrLn ""

-- prints all the solutions for N queens
main1 = do  Main.putStrLn "Enter the number of queens to place : "
            num <- getLine
            let parsenum = read num
            mapM_ printSolution $ queens parsenum



--EX 5


-- Modified from Hangman example from chapter 10 of Programming in Haskell, Graham Hutton, Cambridge University Press, 2016.
-- Author : Julian Rathke, 2018




main = hangman (2*)

hangman :: (Int -> Int) -> IO ()
hangman f = do word <- chooseWord
               let ds = replicate (length word) '-'
               Main.putStrLn ds
               Main.putStrLn "Try to guess it:"
               play word ds (f $ length ds)

secretGetLine :: IO String
secretGetLine = do hSetEcho stdin False
                   xs <- getLine
                   hSetEcho stdin True
                   return xs

putUpdate :: String -> IO String
putUpdate s = do putStr "Your answer so far is : "
                 Main.putStrLn s
                 return s

play :: String -> String -> Int -> IO ()
play word answerSoFar n | n <= 0 = Main.putStrLn "You have used up all of your guesses, game over."
                        | answerSoFar == word = Main.putStrLn "Correct!!"
                        | otherwise =
                          do Main.putStrLn ("You have " ++ (show n) ++ " guesses remaining. Enter a character :  ")
                             guess <- getChar
                             _ <- getChar
                             Main.putStrLn ""
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
                --r <- randomRIO (0::Int,read (numberOfWords)-1)
                r <- getChar
                skipLines (read [r]) handle
                word <- hGetLine handle
                Main.putStrLn ("Secret Word Chosen.")
                return word

skipLines :: Int -> Handle -> IO ()
skipLines 0 h = return ()
skipLines n h = do hGetLine h
                   skipLines (n-1) h


--EX 6

data Prob = T | F | P String | And Prob Prob | Or Prob Prob | Brackets Prob
type Substitution = [ (String, Bool) ]


check :: Maybe a -> a
check Nothing = error ("No binding found for variable ")
check (Just b) = b


evalProb ::Substitution -> Prob -> Bool

evalProb _ T = True
evalProb _ F = False
evalProb s (Main.P string) = check $ lookup string s
evalProb s (And a b) = (evalProb s a) && (evalProb s b)
evalProb s (Or a b) = (evalProb s a) || (evalProb s b)
evalProb s ( Brackets a ) = evalProb s a

truProb :: Parser Prob
truProb = do symbol "T"
             return (T)

falProb :: Parser Prob
falProb = do symbol "F"
             return (F)
varProb :: Parser Prob
varProb = do s <- ident
             return (Main.P s)
andParser :: Parser Prob
andParser = do e1 <- lowerProb
               symbol "&"
               e2 <- lowerProb
               return (And e1 e2)

vParser :: Parser Prob
vParser = do e1 <- evenLowerProb
             symbol "V"
             e2 <- evenLowerProb
             return (Or e1 e2)


prob :: Parser Prob
prob = andParser <|> lowerProb
lowerProb = vParser <|> evenLowerProb
evenLowerProb = varProb <|> truProb <|> falProb


parseProb :: String -> Prob
parseProb = fst . head . (parse prob)
