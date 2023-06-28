module Tests where

import LinearTerm
import BooleanTerm
import WhileTerm
import Probability

-- [DEF] Definição das variáveis
x,y,z :: Var
x = X
y = Y
z = Z

-- [INIT] Variáveis inicializadas a 0
mem :: Memory
mem _ = 0.0

-- [TEST 1]: Atribuir o valor 5.0 à variável x
test1 :: WhileTerm
test1 = Asg x (Leaf (Left 5.0))

-- [TEST 2]: Executar a soma: x = x + 3.0
test2 :: WhileTerm
test2 = Asg x (Add (Leaf (Right x)) (Leaf (Left 3.0)))

-- [TEST 3]: Executar uma sequência de operações: x = x + 2.0; y = x - 1.0
test3 :: WhileTerm
test3 = Seq
        (Asg x (Add (Leaf (Right x)) (Leaf (Left 2.0))))
        (Asg y (Add (Leaf (Right x)) (Leaf (Left (-1.0)))))


-- [TEST 4]: Executar a condição: if x > 0 then y = 1.0 else y = 0.0
test4 :: WhileTerm
test4 = Ife (Comp (Leaf (Right x)) (Leaf (Left 0.0)))
            (Asg y (Leaf (Left 1.0)))
            (Asg y (Leaf (Left 0.0)))

-- [TEST 5]: Executar o ciclo while: while x < 10.0 do x = x + 1.0
test5 :: WhileTerm
test5 = Whi (Comp (Leaf (Right x)) (Leaf (Left 10.0)))
            (Asg x (Add (Leaf (Right x)) (Leaf (Left 1.0))))

main :: IO ()
main = do
    let mem1 = pick(semantic test1 mem)
    putStrLn "Resultado da atribuição:"
    putStrLn ("Memória Final: " ++ show (mem1 x))
    
    let mem2 = pick(semantic test2 mem)
    putStrLn "Resultado da soma:"
    putStrLn ("Memória Final: " ++ show (mem2 x))
    
    let mem3 = pick(semantic test3 mem)
    putStrLn "Resultado da sequência de operações:"
    putStrLn ("Memória Final (x): " ++ show (mem3 x))
    putStrLn ("Memória Final (y): " ++ show (mem3 y))
    
    let mem4 = pick(semantic test4 mem)
    putStrLn "Resultado da atribuição condicional:"
    putStrLn ("Memória Final (x): " ++ show (mem4 x))
    putStrLn ("Memória Final (y): " ++ show (mem4 y))
    
    let mem5 = pick(semantic test5 mem)
    putStrLn "Resultado do loop while:"
    putStrLn ("Memória Final (x): " ++ show (mem5 x))
