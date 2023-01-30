import Data.Maybe
import Data.List 

removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys

allPossibleMatches :: (a -> a -> Bool) -> a -> [a] -> [a]
allPossibleMatches p x xs = filter (p x) xs

helper :: Eq a => (a -> a -> Bool) -> [a] -> [[(a,a)]]
helper p [] = [[]]
helper p (x:xs) = [(x,mx):(mx,x):rest | mx <- allPossibleMatches p x xs, rest <- helper p (removeItem mx xs)]

findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe[(a,a)]

-- if empty list listToMaybe turns it to a Nothing type or to a Just if not empty
findBonding p xs = (listToMaybe . helper p) xs











{-
-- > removeEach [1,2,3,4] == [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
-- produces a list of pairs with an elem x from xs with a list xs / x
takeEachElem :: [a] -> [(a,[a])]

takeEachElem [] = []
takeEachElem (x:xs) = (x,xs):map (fmap (x:)) (takeEachElem  xs)

helper :: (a -> a -> Bool) -> [a] -> [[(a,a)]]
helper p [] = [[]]
helper p (x:xs) = [(x,xx):(xx,x):xys | (xx,xss) <- takeEachElem xs, (p x xx && p xx x), xys <- helper p xss]

-- > findBonding (\x -> \y -> odd(x+y)) [2,3,4,5,6,7] == Just [(2,3),(3,2),(4,5),(5,4),(6,7),(7,6)]
-- > findBonding (\x -> \y -> even(x+y)) [2,3,4,5,6,7] == Nothing

findBonding :: (a -> a -> Bool) -> [a] -> [[(a,a)]]

-- if empty list listToMaybe turns it to a Nothing type or to a Just if not empty
findBonding p xs = helper p xs
-}