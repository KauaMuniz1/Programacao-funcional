import Language.Haskell.TH (prim)
import Control.Concurrent (Chan)
import Control.Monad.Cont (label, cont)
import GHC.Float (fromRat'')
import Distribution.TestSuite (Result(Error), Test (Test))
import GHC.IO.Device (IODevice(dup))
import Data.Char
import Data.Time.Format.ISO8601 (yearFormat)
import Control.Applicative (Alternative(some))
import Data.List
import Data.ByteString.Char8 (split)
import Distribution.Simple.Utils (xargs)
{-
Parte 1 — Funções Básicas
Questão 1 — head, last
Escreva a função primeiroEUltimo :: [a] -> (a, a) que recebe uma lista e devolve uma tupla com o primeiro e o último elemento. Exemplo: primeiroEUltimo [3,2,5,1] = (3,1).
Dica: Use head para o primeiro e last para o último; não precisa de recursão aqui.
-}
primeiroEUltimoZF :: [Int] -> [Int]
primeiroEUltimoZF [] = []
primeiroEUltimoZF lista = [x| x <- [head lista, last lista]]

primeiroEUltimo:: [Int] -> [Int]
primeiroEUltimo lista = head lista : [last lista]

{-
Questão 2 — tail, init
Escreva a função meio :: [a] -> [a] que remove o primeiro E o último elemento de uma lista, devolvendo o que sobrou no meio. Exemplo: meio [3,2,5,1] = [2,5].
Dica: Pense em compor tail com init: o que acontece se você aplicar init no resultado de tail?
-}
meio :: [a] -> [a]
meio [] = []
meio (x:xs) = init xs


meioZF :: [a] -> [a]
meioZF lista = [x | x <- init (tail lista)]

{-
Questão 3 — length
Implemente sua própria versão de length, chamada tamanho :: [a] -> Int, SEM usar a função length pronta. Exemplo: tamanho [3,2,1] = 3.
Dica: Caso base: lista vazia tem tamanho 0. Caso recursivo: 1 + tamanho do resto da lista.
-}

lengthh :: [a] -> Int 
lengthh [] = 0
lengthh (x:xs) = 1+ lengthh xs



{-
Questão 4 — reverse
Implemente sua própria versão de reverse, chamada inverte :: [a] -> [a], SEM usar reverse pronta. Exemplo: inverte [3,2,1] = [1,2,3].
Dica: Caso recursivo: inverta o resto da lista e coloque o primeiro elemento no final (use ++ [x]).
-}

reversee :: [a] -> [a] 
reversee [] = []
reversee (x:xs) = reversee xs ++ [x]

{-
Questão 5 — concat
Implemente minhaConcat :: [[a]] -> [a], que recebe uma lista de listas e junta tudo em uma lista só, sem usar concat pronta. Exemplo: minhaConcat [[2],[3,5]] = [2,3,5].
Dica: Caso recursivo: junte a primeira sublista com a concatenação do restante, usando ++.
-}
minhaConcat :: [[a]] -> [a] 
minhaConcat [] = []
minhaConcat (x:xs) = x ++ minhaConcat xs


{-
Questão 6 — take, drop (juntas)
Escreva removeMeio :: Int -> Int -> [a] -> [a] que recebe dois números i e j e uma lista, e remove os elementos entre as posições i (inclusive) e j (exclusive), mantendo o resto. Exemplo: removeMeio 1 3 [10,20,30,40,50] = [10,40,50] (removeu os índices 1 e 2, que são 20 e 30).
Dica: Combine take i lista com drop j lista, juntando os dois pedaços com ++.
-}
removeMeio :: Int -> Int -> [a] -> [a] 
removeMeio i j lista = take i lista ++ drop j lista 


{-
Questão 7 — splitAt
Escreva metade :: [a] -> ([a], [a]) que divide uma lista em duas partes o mais parecidas possível em tamanho. Se a lista tiver tamanho ímpar, a primeira parte deve ter um elemento extra. Exemplo: metade [1,2,3,4,5] = ([1,2,3],[4,5]).
Dica: Calcule o tamanho da lista, depois use splitAt no ponto certo (lembre de arredondar para cima).
-}

metade :: [a] -> ([a], [a])
metade [] = ([],[])
metade lista = splitAt meio lista 
    where 
        meio = ceiling (fromIntegral (lengthh lista)/2)

{-
Questão 8 — takeWhile
Escreva contaPositivosIniciais :: [Int] -> Int que conta quantos números no INÍCIO da lista são positivos, parando assim que encontrar o primeiro número não positivo (zero ou negativo). Exemplo: contaPositivosIniciais [3,5,2,-1,4] = 3 (porque -1 quebra a sequência, e o 4 depois não conta).
-}
contaPositivosIniciais :: [Int] -> Int 
contaPositivosIniciais [] = 0
contaPositivosIniciais (x:xs) = lengthh(takeWhile (>= 0) xs) 


{-
Questão 9 — takeWhile + drop
Escreva removePrefixoIguais :: [Int] -> [Int] que remove do início da lista todos os elementos iguais ao primeiro elemento, devolvendo o restante. Exemplo: removePrefixoIguais [7,7,7,2,7,3] = [2,7,3].
Dica: Pegue x = head lista, depois use dropWhile (==x) ou conte com takeWhile (==x) e depois drop esse tanto.
-}

removePrefixoIguais :: [Int] -> [Int] 
removePrefixoIguais [] = []
removePrefixoIguais lista = dropWhile (==x) lista
    where 
        x = head lista


{-
Questão 10 — : (cons)
Escreva intercalaZero :: [Int] -> [Int] que insere o número 0 antes de cada elemento da lista original. Exemplo: intercalaZero [3,2,5] = [0,3,0,2,0,5].
Dica: Use o operador : para colocar 0 na frente de cada elemento durante a recursão (ou em uma list comprehension com concat).
-}

intercalaZero :: [Int] -> [Int]
intercalaZero [] = []
intercalaZero (x:xs) = 0: [x] ++ intercalaZero xs

{-
Questão 11 — ++
Escreva juntaTres :: [a] -> [a] -> [a] -> [a] que recebe três listas e devolve elas concatenadas em ordem. NÃO use a função ++ encadeada diretamente nos argumentos — implemente você mesmo a lógica de concatenação com recursão (pode usar ++ apenas dentro da sua própria definição recursiva). Exemplo: juntaTres [1,2] [3] [4,5] = [1,2,3,4,5].
Dica: Pense em como o ++ é definido: concatenar [] com ys dá ys; concatenar (x:xs) com ys dá x : (concatenar xs ys).
-}
juntaTres :: [a] -> [a] -> [a] -> [a] 
juntaTres xs ys zs =  concatena (concatena xs ys) zs 

concatena :: [a] -> [a] -> [a] 
concatena [] ys = ys 
concatena ys [] = ys 
concatena (x:xs) (y:ys) = x: concatena xs (y:ys)


{-
Questão 12 — !! (indexação)
Escreva elementosPares :: [a] -> [a] que devolve os elementos que estão em posições pares da lista (índice 0, 2, 4, ...). Exemplo: elementosPares [10,20,30,40,50] = [10,30,50].
Dica: Você pode usar list comprehension com [lista !! i | i <- [0,2..length lista - 1]], ou recursão olhando de dois em dois elementos.
-}

elementosPares :: [a] -> [a]
elementosPares lista = [lista !! i| i <- [0,2 .. length lista-1]]



palindromo :: String -> Bool
palindromo [] = error "lista vazia"
palindromo (x : xs)
   |reverse (x:xs) == (x : xs) = True
   |otherwise = False



{-
Questão 19 — zip
Escreva somaListas :: [Int] -> [Int] -> [Int] que recebe duas listas de números e devolve uma lista com a soma elemento a elemento. Exemplo: somaListas [1,2,3] [10,20,30] = [11,22,33].
Dica: Use zip para formar pares (a,b) e depois some cada par: [a+b | (a,b) <- zip xs ys].
-}

somaListasZF :: [Int] -> [Int] -> [Int]
somaListasZF [] [] = []
somaListasZF lista1 lista2 = [x + a| (x,a) <- zip lista1 lista2 ]



{-
Questão 1 — Desestruturação de tuplas e filtragem
Você recebe uma lista de tuplas (nome, idade) representando pessoas. Escreva uma expressão ZF que retorne apenas os nomes das pessoas que têm mais de 18 anos E cujo nome começa com a letra 'A'.
Entrada e saída esperada:
pessoas = [("Ana", 22), ("Bruno", 17), ("Alice", 30), ("Alberto", 15), ("Carlos", 25)]
 
-- resultado esperado:
-- ["Ana", "Alice"]
Dica: use desestruturação (nome, idade) diretamente no gerador da ZF, e lembre que head nome dá a primeira letra.
-}







{-
20 questões baseadas nos tópicos estudados (map, filter, foldr, foldl, all, any, aplicação parcial, composição de funções, classes Eq/Ord/Show/Num).


    1. 1. Explique com suas palavras o que é uma função de alta ordem.
    2. 2. Escreva uma função usando map que dobre todos os elementos de uma lista.
    3. 3. Escreva uma função usando map que converta uma lista de Int em seus quadrados.
    4. 4. Escreva uma função usando filter que mantenha apenas os números pares.
    5. 5. Escreva uma função usando filter que mantenha apenas números positivos.
    6. 6. Usando foldr, calcule a soma de uma lista.
    7. 7. Usando foldl, calcule o produto de uma lista.
    8. 8. Explique a diferença entre foldr e foldl com um exemplo.
    9. 9. Escreva uma função usando all que verifique se todos os elementos são maiores que 10.
    10. 10. Escreva uma função usando any que verifique se existe algum elemento negativo.
    11. 11. Escreva uma função que receba f e n e retorne o menor valor de f aplicada em [0..n].
    12. 12. Escreva uma função que teste se todos os valores de f em [0..n] são iguais.
    13. 13. Escreva uma função que teste se todos os valores de f em [0..n] são >=0.
    14. 14. Escreva uma função que teste se os valores de f em [0..n] estão em ordem crescente.
    15. 15. O que é aplicação parcial? Dê um exemplo usando (>=0) ou (+1).
    16. 16. O que é composição de funções? Reescreva map (\x -> (x+1)*2) usando composição quando possível.
    17. 17. O que significa Eq? Quando ela é necessária?
    18. 18. Qual a diferença entre Eq e Ord?
    19. 19. Em quais classes pertencem Int, Char e Bool dentre Eq, Ord, Show e Num?
    20. 20. Escreva a assinatura mais geral de uma função que compare dois valores usando ==.
-}
    

--2 
dobrarElem :: Num a => [a] -> [a]
dobrarElem lista = map (*2) lista

--3 
quadrarElem :: Num a => [a] ->[a]
quadrarElem lista = map (^2) lista



--4
numerosPares :: [Int] -> [Int]
numerosPares = filter even 

--5 
numerosPositivos :: [Int] -> [Int]
numerosPositivos = filter (>0)

--6
somaDeUmaLista :: [Int] -> Int 
somaDeUmaLista = foldr (+) 0

--7
produtoDeUmaLista :: [Int] -> Int 
produtoDeUmaLista = foldl (*) 1

--8
maioresQueDez :: [Int] -> Bool
maioresQueDez = all (> 10)
