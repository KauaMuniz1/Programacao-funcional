--calcular o mmc:

mmc :: Int -> Int -> Int
mmc x y = divi (x * y) (mdc x y)

mdc :: Int -> Int -> Int
mdc x y
  |x == y = x
  |x > y = mdc (x-y) y
  |otherwise = mdc y x

divi :: Int -> Int -> Int
divi x y
    |x == y = 1
    |x > y = 1 + divi (x-y) y
    |otherwise = 0


--exercicios tópico 4.3.3
--1. Dê a definição da função todosQuatroIguais do tipo Int -> Int -> Int -> Int -> Bool
--que dá o resultado True se seus quatro argumentos forem iguais.
todosIguais :: Int -> Int -> Int -> Int -> Bool
todosIguais a b c d 
    |a == b && b == c && c == d = True
    |otherwise = False



--3. O que está errado com a definição da função todosDiferentes abaixo?
--todosDiferentes n m p = ((n/ = m) && (m/ = p))
--resposta: não verifica se todos são diferentes pois verifica se o n é diferente de m e se o m é diferente de p, mas não verifica se o n é diferente de p


--4. Projete um teste adequado para a função todosIguais, considerando a função
--teste :: Int -> Int -> Int -> Bool
--teste n m p = ((nm+p) == 3*p)+
--Esta função se comporta da mesma forma que a função todosIguais para o seu teste de
--dados? Que conclusão você tira sobre os testes em geral?
--resposta: o teste nao funciona caso seja n = 1 m = 2 p = 3, pois (1*2+3) != 3 * 3


--Dê uma definição para a função quantosIguais aplicada a três entradas inteiras e que
--retorna quantas delas são iguais
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c 
  |(a == b) && (b == c) = 3
  |(a == b) || (a == c) || (b == c) = 2
  |otherwise = 0


