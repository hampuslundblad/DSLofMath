data SET v = Empty
            | Singleton v
            | Union (SET v) (SET v)
            | Intersection (SET v) (SET v)
            | Var v
 deriving Show

data PRED v = Elem v (SET v)
            | Subset v (SET v)
            | And (PRED v) (PRED v) 
            | Or (PRED v) (PRED v)
            | Implies (PRED v) (PRED v)
            | Not (PRED v)

newtype Set = S [Set]
 deriving Show

eval :: Eq v => Env v Set -> SET v -> Set
eval env Empty = S []
eval _ (Singleton var) =  S []
eval env (Var a) = lookup' (Var a) env 
eval env (Union a1 a2) =    S ((lookup' a1 env):(lookup' a2 env):[])
--eval env (Intersection a1 a2) = [(v,d) | (v,d) <- envG

lookup' ::Eq v => SET v -> Env v Set -> Set   
lookup' (Var a) ((var,dom) : envs) 
                        | a == var   = dom
                        | otherwise  = lookup' (Var a) envs
lookup' a [] = S []

--evalEnv :: Eq v => Env v s -> (v -> Maybe s)
--evalEnv vss var = findFst vss
--    where findFst ((v, s) : vss)
 --             | var v = Just s
 --             | otherwise = findFst vss
 --           findFst [ ] = Nothing


type Env var dom = [(var, dom)]




