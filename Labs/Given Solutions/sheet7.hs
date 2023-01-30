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

