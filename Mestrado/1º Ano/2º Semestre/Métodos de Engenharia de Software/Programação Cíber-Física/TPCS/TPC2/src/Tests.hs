module Tests where

import LinearTerm
import BooleanTerm
import WhileTerm

-- [DEF] Definição das variáveis
x,y,z :: Vars
x = X
y = Y
z = Z

-- [INIT] Variáveis inicializadas a 0
mem :: Vars -> Double
mem _ = 0.0

-- [TESTE1]: Atribuir o valor 5.0 à variável x
test1 = Asg x (Leaf (Left 5.0))

{- [TESTE2]: Executar uma sequência de dois programas
   * Primeiro programa: Atribuição do valor 5.0 à variável x
   * Segundo programa: Escrita da lista de mensagens [13232,0,12,123,98], juntamente
                       com a atribuição do valor 2.0 à variável y.
-}
test2 = Seq (Asg x (Leaf (Left 5.0))) (Write [13232,0,12] (Write [123,98] (Asg y (Leaf (Left 2.0)))))

{- [TESTE3]: Comparar dos valores de duas variáveis
   * Condição: Se o valor da variável x for menor ou igual ao da variável y
   * Caso afirmativo: Atribuição do valor 1.0 à variável z
   * Caso negativo: Atribuição do valor 2.0 à variável z
-}
test3 = Ife (Comp (Leaf (Right x)) (Leaf (Right y))) (Asg z (Leaf (Left 1.0))) (Asg z (Leaf (Left 2.0)))

{- [TESTE4]: Executar um ciclo while
   * Condição: Se o valor da variável x for menor ou igual ao da variável y
   * Caso afirmativo: Atribuição do resultado da soma dos valores das variáveis x e y à variável x
-}
test4 = Whi (Comp (Leaf (Right x)) (Leaf (Right y))) (Asg x (Add (Leaf (Right x)) (Leaf (Right y))))

{- [TESTE5]: Escrever uma lista de mensagens
   * Lista de mensagens: [1,2,3,4,5]
   * Programa: Atribuir o valor 5.0 à variável x
-}
test5 = Write [1,2,3,4,5] test1

main :: IO ()
main = do
    let (msgs1, mem1) = semWhile test1 mem
    putStrLn "Resultado do teste 1:"
    putStrLn ("Mensagens: " ++ show msgs1)
    putStrLn ("Memória X: " ++ show (mem1 x))
    putStrLn ("Memória Y: " ++ show (mem1 y))
    putStrLn ("Memória Z: " ++ show (mem1 z))
    putStrLn ""

    let (msgs2, mem2) = semWhile test2 mem1
    putStrLn "Resultado do teste 2:"
    putStrLn ("Mensagens: " ++ show msgs2)
    putStrLn ("Memória X: " ++ show (mem2 x))
    putStrLn ("Memória Y: " ++ show (mem2 y))
    putStrLn ("Memória Z: " ++ show (mem2 z))
    putStrLn ""

    let (msgs3, mem3) = semWhile test3 mem2
    putStrLn "Resultado do teste 3:"
    putStrLn ("Mensagens: " ++ show msgs3)
    putStrLn ("Memória X: " ++ show (mem3 x))
    putStrLn ("Memória Y: " ++ show (mem3 y))
    putStrLn ("Memória Z: " ++ show (mem3 z))
    putStrLn ""

    let (msgs4, mem4) = semWhile test4 mem3
    putStrLn "Resultado teste 4:"
    putStrLn ("Mensagens: " ++ show msgs4)
    putStrLn ("Memória X: " ++ show (mem4 x))
    putStrLn ("Memória Y: " ++ show (mem4 y))
    putStrLn ("Memória Z: " ++ show (mem4 z))
    putStrLn ""

    let (msgs5, mem5) = semWhile test5 mem4
    putStrLn "Resultado teste 5:"
    putStrLn ("Mensagens: " ++ show msgs5)
    putStrLn ("Memória X: " ++ show (mem5 x))
    putStrLn ("Memória Y: " ++ show (mem5 y))
    putStrLn ("Memória Z: " ++ show (mem5 z))
    putStrLn ""
