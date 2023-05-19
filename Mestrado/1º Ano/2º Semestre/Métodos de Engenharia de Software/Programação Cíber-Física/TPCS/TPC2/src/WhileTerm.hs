module WhileTerm where

import LinearTerm
import BooleanTerm

data WhileTerm = Asg Vars LinearTerm
    | Write [Int] WhileTerm
    | Seq WhileTerm WhileTerm
    | Ife BooleanTerm WhileTerm WhileTerm
    | Whi BooleanTerm WhileTerm deriving Show

-- Sendo d o novo valor a atribuir à variável x, esta função atualiza o valor em memória associado a x.
asgMem :: Vars -> Double -> (Vars -> Double) -> (Vars -> Double)
asgMem x d mem = \v -> if v == x then d else mem v


{- A função recebe um termo WhileTerm e a memória inicial.
   De acordo com as regras de semântica (asg, write, seq, if1, if2, wh1, wh2),
   a função avaliará o termo e devolverá não só o output do programa (mem),
   como também a nova lista de mensagens resultante.
-}
semWhile :: WhileTerm -> (Vars -> Double) -> ([Int], (Vars -> Double))

{- [ATRIBUIÇÃO] Primeiro, calcula-se o valor do termo linear passado (t),
   atualizando a memória com a atribuição do valor r à variável v.
   A lista de mensagens retornada é vazia.
-}
semWhile (Asg v t) mem = let r = semLinear t mem
                            in ([], asgMem v r mem)

{- [WRITE] Para além de atualizar a memória,
   é feita uma concatenação das mensagens que já existiam em memória (msgs)
   com o novo conjunto de mensagens que foram escritas (msg).
-}
semWhile (Write msg p) mem = let (msgs, mem') = semWhile p mem
                                in (msg ++ msgs, mem')

{- [SEQUÊNCIA] Para além de atualizar a memória,
   é feita uma concatenação do conjunto de mensagens do programa p (msgs1)
   com o conjunto de mensagens do programa q (msgs2).
-}
semWhile (Seq p q) mem = let (msgs1, mem') = semWhile p mem
                             (msgs2, mem'') = semWhile q mem'
                            in (msgs1 ++ msgs2, mem'')

{- [CONDICIONAL] Se a condição for verdade,
   então é retornado o conjunto de mensagens do programa p (msgs1),
   caso contrário é retornado o conjunto de mensagens do programa q (msgs2).
-}
semWhile (Ife b p q) mem = if (boolTerm b mem) then semWhile p mem else semWhile q mem

{- [CICLO] Se a condição for verdade, para além de ser atualizada a memória,
   o programa é executado e, por isso, são guardadas as suas mensagens (msgs1)
   e executado uma nova iteração do ciclo, guardando também as suas mensagens (msgs2).
   No final, estes dois conjuntos de mensagens são concatenados.
   Caso a condição não seja verdade, o programa não é executado, a memória não é
   atualizada e é retornado um conjunto vazio de mensagens.
-}
semWhile (Whi b p) mem = if boolTerm b mem
                          then let (msgs1, mem') = semWhile p mem
                                   (msgs2, mem'') = semWhile (Whi b p) mem'
                                in (msgs1 ++ msgs2, mem'')
                          else ([], mem)