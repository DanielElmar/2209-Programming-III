

data Expr = Var String | Lam String Expr | App Expr Expr | Cl String Expr Environment 
     deriving (Eq,Show,Read)

--EX 1

type Environment = [(String, Expr)]



lookup :: (Eq a) => a -> [(a,b)]-> Maybe b
lookup v [] = Nothing
lookup v ((a,b):xs) | a == v = Just b
                    | otherwise = Main.lookup v xs


-- EX 2

data Frame = HoleApp Expr Enviroment | AppHole Expr
type Continuation = [Frame]
type Configuration = ( Expr , Enviroment, Continuation)


-- EX 3

update:: Enviroment -> Stiring -> Expr -> Enviroment
update env x v = (x, v) : env

unpack :: Cl -> (Expr, Environment)
unpack (Cl x e env1) = ((Lam x e) , env1)
unpack _ = error "Cannont unpack closure"


checkLookup :: Maybe Expr -> Expr
checkLookup (Nothing) = error "Unbound variable found"
checkLookup (Just e) = e

isValue :: Expr -> Bool
isValue (Cl _ _ _) = True
isValue _ = False


eval1 :: Configuration -> Configuration 

--R1
eval1 Var x, env, kon = ( expr , env2, kon)
                           where (Cl x expr env2) = checkLookup $ lookup x env

--R2
eval1 (App e1 e2, env, kon) = (e1 , env , ( (HoleApp e2 env) : kon))

--R3
eval1 (Lam x e1), env, kon = ( Cl( x e1 env), env, kon )

--R4
eval1 w@(Cl x e1 env), env1, (HoleApp e2 env) : kon  = ( e, env2,  (AppHole w) : kon  )

--R5
eval1 w@(Cl x1 e1 env) , env1, (AppHole (Cl x e env2) ) : k )  = ( e, update ev2 x w, kon)  )


-- Rule for terminated evaluations
eval1 c@((Cl _ _ _),_,[]) = c
-- Rule for runtime errors, if no cases above match then something has gone wrong
eval1 (_) = error "Evaluation Error"


--Ex 4


-- Peforms multiple steps of call-by-? reduction until no change in term is observed
reductions :: (Expr -> Expr) -> Expr -> [ (Expr, Expr) ]
reductions ss e = [ p | p <- zip evals (tail evals) ]
   where evals = iterate ss e

eval :: Lam -> Expr

eval x e1 = fst . head . dropWhile (uncurry (/=)) . reductions eval1 (x e1, [], [])






{-

Given Solution FOR LAMDA INTERPRETER


-- Example Lambda calculus interpreter

data Expr = Var String | Lam String Expr | App Expr Expr
  deriving (Eq, Show, Read)

free :: String -> Expr -> Bool
free x (Var y) =  x == y
free x (Lam y e) | x == y = False
free x (Lam y e) | x /= y = free x e
free x (App e1 e2)  = (free x e1) || (free x e2)

rename x e | free (x ++â€\â€™â€) e = rename (x++â€\â€™â€) e
           | otherwise = (x++â€\â€™â€)

subst :: Expr -> String ->  Expr -> Expr
subst (Var x) y e | x == y = e
subst (Var x) y e | x /= y = Var x
subst (Lam x e1) y e  |  x /= y && not (free x e)  = Lam x (subst e1 y e)
subst (Lam x e1) y e  |  x /= y &&     (free x e)  = let x' = (rename x e1) in subst (Lam x' (subst e1 x (Var x'))) y e
subst (Lam x e1) y e  | x == y  = Lam x e1
subst (App e1 e2) y e = App (subst e1 y e) (subst e2 y e) 

-- Performs a single step of call-by-name reduction
eval1cbn :: Expr -> Expr
eval1cbn (Lam x e) = (Lam x e)
eval1cbn (App (Lam x e1) e2) = subst e1 x e2
eval1cbn (App e1 e2) = App (eval1cbn e1) e2

-- Peforms multiple steps of call-by-name reduction until no change in term is observed
reductions :: (Expr -> Expr) -> Expr -> [ (Expr, Expr) ]
reductions ss e = [ p | p <- zip evals (tail evals) ]
   where evals = iterate ss e

eval :: (Expr -> Expr) -> Expr -> Expr
eval ss = fst . head . dropWhile (uncurry (/=)) . reductions ss

trace :: (Expr -> Expr) -> Expr -> [Expr]
trace ss  = (map fst) . takeWhile (uncurry (/=)) .  reductions ss

eval1cbv :: Expr -> Expr
eval1cbv (Lam x e) = (Lam x e)
eval1cbv (App (Lam x e1) e@(Lam y e2)) = subst e1 x e
eval1cbv (App e@(Lam x e1) e2) = App e (eval1cbv e2)
eval1cbv (App e1 e2) = App (eval1cbv e1) e2

evalcbn = eval eval1cbn
tracecbn = trace eval1cbn
evalcbv = eval eval1cbv
tracecbv = trace eval1cbv


myid = Lam "z" (Var "z")

-- This function builds a "pair" expression tree from two subtrees
pair :: Expr -> Expr -> Expr
pair e1 e2 = Lam "v" (App (App (Var "v") e1) e2)

-- These are lambda terms that select the first or second input
tru = Lam "x" (Lam "y" (Var "x") )
fls = Lam "x" (Lam "y" (Var "y") )

-- These build the fst and snd functions to extract from our pair encoding
myfst e =  (App e tru)
mysnd e =  (App e fls)

term1 = myfst (pair tru myid)
term2 = mysnd (pair myid term1)

-- While True Loop

ww = Lam "x" (App (Var "x") (Var "x"))
omega = App ww ww




ACTUAL SOLUTION

--COMP2209 Autumn 2017
--Sample solutions to Exercise Sheet 7
import Data.List

-- Exercise One

data Expr = Var String | Lam String Expr | App Expr Expr | Cl String Expr Environment
  deriving (Eq, Show, Read)

type Environment = [ (String,Expr) ]

--lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
--lookup key [] =  Nothing
--lookup key ((x,y):ps)  | key == x =  Just y
--                       | otherwise = lookup key ps


-- Exercise Two

data Frame = HoleApp Expr Environment | AppHole Expr
type Kontinuation = [ Frame ]
type Configuration = (Expr,Environment,Kontinuation)


-- Exercise Three

-- Function to update an environment with a new binding
update :: Environment -> String -> Expr -> Environment
update env x e = (x,e) : env

-- Function to unpack a closure to extract the underlying lambda term and environment
unpack :: Expr -> (Expr,Environment)
unpack (Cl x e env1) = ((Lam x e) , env1)
unpack _ = error "Cannont unpack closure"

checkLookup :: Maybe Expr -> Expr
checkLookup (Nothing) = error "Unbound variable found"
checkLookup (Just e) = e

isValue :: Expr -> Bool
isValue (Cl _ _ _) = True
isValue _ = False

--Small step evaluation function
eval1 :: Configuration -> Configuration 
-- Rule R1
eval1 ((Var x),env,k) = (e',env',k) 
      where (e',env') = (unpack $ checkLookup $ lookup x env)
-- Rule R2
eval1 ((App e1 e2),env,k) = (e1,env, (HoleApp e2 env) : k)
-- Rule R3
eval1 ((Lam x e),env,k) = ((Cl x e env), env, k)
-- Rule R4
eval1 (w,env1,(HoleApp e env2):k ) | isValue w = (e, env2, (AppHole w) : k)
-- Rule R5
eval1 (w,env1,(AppHole (Cl x e env2) ) : k ) | isValue w  = (e, update env2 x w, k)
-- Rule for terminated evaluations
eval1 c@((Cl _ _ _),_,[]) = c
-- Rule for runtime errors, if no cases above match then something has gone wrong
eval1 (_) = error "Evaluation Error"

-- Exercise Four

-- Function to iterate the small step reduction to termination
evalLoop :: Expr -> Expr 
evalLoop e = fst $ unpack $ evalLoop' (e,[],[])
  where evalLoop' (e,env,k) = case eval1 (e,env,k) of 
                                (w@(Cl _ _ _), _ , []) -> w
                                c -> evalLoop' c

-- Exercise Five
testTerm :: Expr
testTerm = read "App (Lam \"v\"(App (App (Var \"v\") (Lam \"z\" (Var \"z\")))(App (Lam \"v\" (App (App (Var \"v\") (Lam \"x\" (Lam \"y\" (Var \"x\")))) (Lam \"z\" (Var \"z\")))) (Lam \"x\" (Lam \"y\" (Var \"x\")))))) (Lam \"x\" (Lam \"y\" (Var \"y\")))"

-}








