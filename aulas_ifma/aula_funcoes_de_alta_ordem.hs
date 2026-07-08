import GHC.Exts.Heap (GenClosure(fun))
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
minFunc :: (Int -> Int ) -> Int-> Int 
minFunc func lista = minimum (map func [0..lista])

--q5 
todosIguais:: (Int -> Int) -> Int -> Bool 
todosIguais f n = all (== head valores) valores
    where 
        valores = map f [0.. n]


--q6
maioresQueZero ::  (Int -> Int) -> Int -> Bool 
maioresQueZero f n = all (>= 0) valores 
    where 
        valores = map f [0..n] 


--versao melhorada{
-- maioresQueZero :: (Ord a,Num a) => (Float -> a) -> Float -> Bool 
-- maioresQueZero f n = all (>= 0) valores 
--     where 
--         valores = map f [0..n] 
--}

--q7
ordemCrescente :: (Int -> Int) -> Int -> Bool 
ordemCrescente func x = crescente lista 
    where 
        lista = map func [0..x]


crescente :: [Int] -> Bool 
crescente [] = True 
crescente [x] = True 
crescente (x:xs) 
    |x >= head xs = crescente xs 
    |otherwise = False 
