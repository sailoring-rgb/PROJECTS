module WhileTerm where

import Probability
import LinearTerm
import BooleanTerm

type Memory = Var -> Double

data WhileTerm = Asg Var LinearTerm
    | Sum Float WhileTerm WhileTerm
    | Seq WhileTerm WhileTerm
    | Ife BooleanTerm WhileTerm WhileTerm
    | Whi BooleanTerm WhileTerm deriving Show

por :: Float -> (Dist a, Dist a) -> Dist a
por p (x, y) = do
  a <- x
  b <- y
  choose p a b

-- Sendo d o novo valor a atribuir à variável x, esta função atualiza o valor em memória associado a x.
asgMem :: Var -> Double -> Memory -> Memory
asgMem x d mem = \v -> if v == x then d else mem v

semantic :: WhileTerm -> Memory -> Dist Memory
semantic (Asg x t) mem = do
                          let r = semLinear t mem
                          return (asgMem x r mem)

semantic (Sum prob p q) mem = por prob (semantic p mem, semantic q mem)

semantic (Seq p q) mem = do 
                          r <- semantic p mem
                          semantic q r

semantic (Ife b p q) mem = do 
                              let condition = boolTerm b mem
                              if condition
                                then semantic p mem
                                else semantic q mem

semantic (Whi b p) mem = do 
                          let condition = boolTerm b mem
                          if condition
                            then do
                              r <- semantic p mem
                              semantic (Whi b p) r
                          else return mem