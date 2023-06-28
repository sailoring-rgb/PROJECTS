module BooleanTerm where

import LinearTerm

data BooleanTerm = Comp LinearTerm LinearTerm
        | Neg BooleanTerm
        | And BooleanTerm BooleanTerm
        deriving Show

boolTerm :: BooleanTerm -> (Var -> Double) -> Bool
boolTerm (Comp t1 t2) m = let r1 = semLinear t1 m
                              r2 = semLinear t2 m
                          in r1 <= r2
boolTerm (Neg b) m = let v = boolTerm b m
                     in not v
boolTerm (And b1 b2) m = let v1 = boolTerm b1 m
                             v2 = boolTerm b2 m
                         in v1 && v2
