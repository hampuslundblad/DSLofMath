data SET v = Empty
            | Singleton v
            | Union (SET v) (SET v)
            | Intersection (SET v) (SET v)
            | Var v

data PRED v = Elem v (SET v)
            | Subset v (SET v)
            | And (PRED v) (PRED v) 
            | Or (PRED v) (PRED v)
            | Implies (PRED v) (PRED v)
            | Not (PRED v)

newtype Set = S [Set]

eval :: Eq v => Env v Set -> SET v -> Set
eval ((a,b):xs) s = S (b:(eval xs s):[])

type Env var dom = [(var, dom)]
