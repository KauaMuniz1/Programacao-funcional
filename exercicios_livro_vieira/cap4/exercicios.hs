import Data.Char
import Distribution.Simple.Utils (xargs)
import Distribution.Simple.Build (repl)
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


{-4. Projete um teste adequado para a função todosIguais, considerando a função
teste :: Int -> Int -> Int -> Bool
teste n m p = ((nm+p) == 3*p)+
Esta função se comporta da mesma forma que a função todosIguais para o seu teste de
dados? Que conclusão você tira sobre os testes em geral?
resposta: o teste nao funciona caso seja n = 1 m = 2 p = 3, pois (1*2+3) != 3 * 3
-}

--Dê uma definição para a função quantosIguais aplicada a três entradas inteiras e que
--retorna quantas delas são iguais
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c 
  |(a == b) && (b == c) = 3
  |(a == b) || (a == c) || (b == c) = 2
  |otherwise = 0



{-Exemplo. Vamos construir um programa que envolva operações com inteiros. Imaginemos uma
empresa de vendas que necessita de respostas para as questões a seguir, para fundamentar suas
decisões:
  • Questão 1: Qual o total de vendas desde a semana 0 até a semana n?
  • Questão 2: Qual a maior venda semanal entre as semanas 0 e n?
  • Questão 3: Em que semana ocorreu a maior venda?
  • Questão 4: Existe alguma semana na qual nada foi vendido?
  • Questão 5: Em qual semana não houve vendas? (se houve alguma).

-}

--Questão 1: 
venda :: Int -> Int
venda 0 = 7
venda 1 = 3
venda 2 = 5
venda 3 = 0
venda 4 = 10
venda 5 = 2
venda 6 = 0
venda 7 = 9

totalVendas :: Int -> Int 
totalVendas n
  |n == 0 = venda 0
  |otherwise = venda n + totalVendas(n-1) 


--Questão 2:
maiorVenda:: Int -> Int
maiorVenda n 
  |n == 0 = venda n
  |maiorVenda(n-1) >= venda n = maiorVenda(n-1)
  |otherwise = venda n


--Outra forma de fazer 
maximo :: Int -> Int -> Int
maximo x y 
  |x >= y = x
  |otherwise = y

maiorVenda2 :: Int -> Int
maiorVenda2 n 
  |n == 0 = venda 0
  |otherwise = maximo (maiorVenda2(n-1)) (venda n)



{-
Exercı́cios:
1. Defina uma função para encontrar a semana em que ocorreu a venda máxima entre a
semana 0 e a semana n. O que sua função faz se houver mais de uma semana com vendas
máximas?
2. Defina uma função para encontrar uma semana sem vendas entre as semanas 0 e n. Se
não existir tal semana, o resultado deve ser n + 1.
3. Defina uma função que retorne o número de semanas sem vendas (se houver alguma).
4. Defina uma função que retorna o número de semanas nas quais foram vendidas s unidades,
para um inteiro s ≥ 0. Como você usaria esta solução para resolver o problema 3?
5. Teste as funções que usam vendas com a definição vendas n = n ′ mod′ 2 + (n + 1) ′ mod′ 3
-}

--questao 2
semanaSemVenda :: Int -> Int
semanaSemVenda n
  |venda n == 0 = n
  |n == 0 = n+1
  |otherwise = semanaSemVenda(n-1)


--questao 3
numSemanaSemVendas :: Int -> Int
numSemanaSemVendas n
  |n == 0 && venda n == 0 = 1
  |n == 0 && venda n /= 0 = 0
  |venda n == 0 = 1+numSemanaSemVendas(n-1)
  |otherwise = numSemanaSemVendas(n-1)


--questao 4
numSemanaComSVendas :: Int ->Int -> Int
numSemanaComSVendas s n
  |n == 0 && venda n == s = 1
  |n == 0 && venda n /= s = 0
  |venda n == s = 1 + numSemanaComSVendas s (n-1)
  |otherwise = numSemanaComSVendas s (n-1) 




--Exercicios tipos booleanos
{-
1. Dê a definição de uma função nAnd :: Bool -> Bool -> Bool que dá o resultado T rue,
exceto quando seus dois argumentos são ambos T rue.
682. Defina uma função numEquallMax :: Int -> Int -> Int -> Int onde numEquall-
Max n m p retorna a quantidade de números iguais ao máximo entre n, m e p.
3. Como você simplificaria a função
funny x y z
|x > z = True
|y >= x = False
|otherwise = True
-}

--Questao 1
nAnd :: Bool -> Bool -> Bool
nAnd x y
  |x == True && y == True = False
  |otherwise = True



--Exercicios do tipo String
{-
1. Defina uma função para converter letras minúsculas em maiúsculas e que retorne o próprio
caractere se a entrada não for um caractere minúsculo.
2. Defina uma função charParaInt :: Char -> Int que converte um dı́gito em seu valor
(por exemplo, ’8’ em 8). O valor de um caractere não dı́gito deve ser 0 (zero).
3. Defina uma função imprimeDigito :: Char -> String que converte um dı́gito em sua
representação em português. Por exemplo, ’5’ deve retornar “cinco”.
4. Defina uma função romanoParaString :: Char -> String que converte um algarismo
romano em sua representação em Português. Por exemplo, romanoP araString ′ V ′ =
“cinco′′ .
5. Defina uma função emTresLinhas :: String -> String -> String -> String que toma
três strings como argumento e retorna uma única string mostrando as três em linhas se-
paradas.
6. Defina uma função replica :: String -> Int -> String que toma uma string e um
número natural n e retorna n cópias da string, todas juntas. Se n for 0, o resultado deve
ser a string vazia (), se n for 1, retorna a própria string.
-}

--questao 1
toMaiuscula :: Char -> Char
toMaiuscula c
  |c >= 'a' && c <= 'z' = chr(ord c -32)
  |otherwise = c


--questao 2
charParaInt :: Char -> Int 
charParaInt x
  |x >= '0' && x <= '9' = ord x - ord '0'
  |otherwise = 0


--questao 4 
romanoParaString :: Char -> String
romanoParaString x 
  |x == 'v' = "cinco"
  |x == 'd' = "dez"
  |x == 'i' = "um"
  |x == 'c' = "cem"
  |x == 'l' = "cinquenta"
  |x == 'm' = "mil"


replica :: String -> Int -> String
replica x 0 = []
replica x 1 = x
replica x y 
  |y > 1 = replica x (y-1) ++ x