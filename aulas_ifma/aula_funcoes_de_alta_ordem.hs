{-
▪Defina funções que tomem uma lista de inteiros L e
▪Retorne a lista dos quadrados dos elementos de L
▪Retorne a soma dos quadrados dos elementos de L
▪Verifique se todos os elementos da lista são positivos


▪Escreva funções que calculem
▪O valor mínimo de uma função aplicada a uma lista de
inteiros de 0 a n
▪Teste se o resultado de uma função f aplicada sobre
as entradas de 0 a n são todas iguais
▪Teste se todos os valores de f aplicada às entradas
de 0 a n são maiores ou iguais a 0
▪Teste se todos os valores de f aplicada às entradas
de 0 a n estão em ordem crescente
-}

--q1
retornaQuadrados:: [Int] -> [Int]
retornaQuadrados = map (^2) 

--q2
retornaSomaQuadrados :: [Int] -> Int 
retornaSomaQuadrados lista = foldr (+) 0 (retornaQuadrados lista) 

--q3 
verificaPositivo :: [Int] -> Bool 
verificaPositivo = all ( > 0) 

--q4