{-
1. Escreva uma função polimórfica chamada mapeiaCondicional que receba como parâmetro uma
função de retorno booleano e duas outras funções, além de uma lista de um tipo qualquer. Para
cada elemento da lista, se a primeira função retornar True o elemento deve ser transformado pela
segunda função, caso contrário deve ser transformado pela terceira função.
Exemplo:
mapeiaCondicional even (*2) (+1) [1,2,3,4] = [2,4,4,8]
-}
mapeiaCondicional :: Num a => (a -> Bool) -> (a -> a) -> (a -> a) -> [a] -> [a]
mapeiaCondicional f1 f2 f3 lista = map (\x -> if f1 x == True then f2 x else f3 x) lista




{-
3. Escreva uma função polimórfica chamada contaSatisfazendoAninhada que receba como
parâmetro uma função de retorno booleano e uma lista de listas de um tipo qualquer. A função deve
retornar a quantidade total de elementos, dentre todas as sublistas, que satisfazem a condição.
Exemplo:
contaSatisfazendoAninhada even [[1,2,3],[4,5],[6]] = 3
-}
contaSatisfazendoAninhada :: Num a => (a -> Bool) -> [[a]] -> Int 
contaSatisfazendoAninhada func = sum . map (length . filter func)


{-
4. Escreva uma função polimórfica chamada particiona que receba como parâmetro uma função de
retorno booleano e uma lista de um tipo qualquer. A função deve retornar uma tupla contendo, na
primeira posição, os elementos que satisfazem a condição e, na segunda posição, os elementos
que não satisfazem, mantendo a ordem original.
Exemplo:
particiona even [1,2,3,4,5,6] = ([2,4,6],[1,3,5])
-}
particiona :: (a -> Bool) -> [a] -> ([a],[a])
particiona func lista = (filter func lista, filter (not . func) lista)

--usando zf
particionaZF :: (a -> Bool) -> [a] -> ([a],[a])
particionaZF func lista = ([x | x <- filter func lista], [y | y <- filter (not .func) lista])


{-
5. Escreva uma função polimórfica chamada removeAdjacentesIguais que receba como parâmetro
uma lista de um tipo qualquer. A função deve remover elementos duplicados que estejam lado a
lado, mantendo apenas a primeira ocorrência de cada sequência.
Exemplo:
removeAdjacentesIguais "aabbbcca" = "abca"
-}
removeAdjacentesIguais :: Eq a => [a] -> [a]
removeAdjacentesIguais [] = []
removeAdjacentesIguais [x] = [x]
removeAdjacentesIguais (x:xs)
    |x == head xs = removeAdjacentesIguais xs 
    |otherwise = x : removeAdjacentesIguais xs

{-
6. Escreva uma função polimórfica chamada iteraAte que receba como parâmetro uma função, uma
função de retorno booleano e um valor inicial de um tipo qualquer. A função deve aplicar
repetidamente a primeira função ao valor, acumulando cada resultado em uma lista, até que o
resultado satisfaça a condição (o elemento que satisfaz a condição deve ser incluído na lista).
Exemplo:
iteraAte (*2) (>20) 1 = [1,2,4,8,16,32]
-}

iteraAte :: Eq a => (a -> a) -> (a -> Bool) -> a -> [a] 
iteraAte f1 f2 valor 
    |f2 valor == True = [valor]
    |otherwise = valor : iteraAte f1 f2 (f1 valor)  




{-
19. Escreva uma função polimórfica chamada primeiroQueSatisfaz que receba como parâmetro uma
função de retorno booleano e uma lista de um tipo qualquer. A função deve retornar, encapsulado
em Just, o primeiro elemento que satisfaz a condição, ou Nothing caso nenhum elemento
satisfaça.
Exemplo:
primeiroQueSatisfaz even [1,3,5,6,7] = Just 6
primeiroQueSatisfaz even [1,3,5] = Nothing
-}

primeiroQueSatisfaz :: Num a => (a -> Bool) -> [a] -> [a]
primeiroQueSatisfaz _[] = []
primeiroQueSatisfaz func lista = filter func lista