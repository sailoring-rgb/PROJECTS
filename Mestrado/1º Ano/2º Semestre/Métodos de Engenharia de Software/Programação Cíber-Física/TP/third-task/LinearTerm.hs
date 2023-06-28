module LinearTerm where

data Var = X | Y | Z deriving (Show, Eq)

data LinearTerm = Leaf (Either Double Var)
    | Scl Double LinearTerm
    | Add LinearTerm LinearTerm deriving Show

semLinear :: LinearTerm -> (Var -> Double) -> Double
semLinear (Leaf (Left r)) m = r
semLinear (Leaf (Right x)) m = m x
semLinear (Scl d t) m = let r = semLinear t m
                            in d * r
semLinear (Add t1 t2) m = let r1 = semLinear t1 m
                              r2 = semLinear t2 m
                            in r1 + r2