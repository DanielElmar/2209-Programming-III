--COMP2209 Autumn 2017
--Sample solutions to Exercise Sheet 6, Ex 1,2
import Data.Graph

-- Exercise One

evenedges = [ (n,n+1) | n <-[0,2..998] ]
oddedges = [ (n, n `div` 5) | n <- [1,3..999] ]

graph = buildG (0,1000) (evenedges++oddedges)

isReachable :: Int -> Int -> Bool
isReachable n m = elem m (reachable graph n)

-- Exercise Two

data GGraph a = GNode a (GGraph a) deriving Show

mkGraph :: [ (a, Int) ] -> [GGraph a]
mkGraph table = table'
 where --table' :: [GGraph a]
       table' = map (\(x,n) -> GNode x (table'!!n)) table

table = merge evenedges oddedges
  where merge (x:xs) ys = x : (merge ys xs)
        merge [] ys = ys

graphCD = mkGraph table

nextNode :: GGraph a -> GGraph a
nextNode (GNode a g) = g

nodeID :: GGraph a -> a
nodeID (GNode v g) = v

isReachableCD :: Int -> Int -> Bool
isReachableCD n m = isReachableCD' (graphCD!!n) m []
  where isReachableCD' node m visited | (nodeID node) == m = True
        isReachableCD' node m visited | elem (nodeID node) visited = False 
        isReachableCD' node m visited | otherwise = isReachableCD' (nextNode node) m ((nodeID node):visited)
