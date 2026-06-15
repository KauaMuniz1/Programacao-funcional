import Data.Char
import Distribution.Simple.Utils (xargs)
import Distribution.Simple.Build (repl)
import Control.Applicative (Alternative(empty))
import Distribution.PackageDescription (qualifiedExeName)
import Data.Time.Format.ISO8601 (yearFormat)
import GHC.Base (VecElem(Int16ElemRep))
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



{-
xercı́cios:
1. Dê uma definição de uma função tabeladeFatoriais :: Int -> Int -> String que
mostre, em forma de tabela, os fatoriais dos inteiros de m até n, inclusive de ambos.
2. Refaça o exercı́cio anterior adimitindo a possibilidade de entradas negativas e de que o
segundo argumento seja menor que o primeiro.
-}

--questao 1

tabeladeFatoriais :: Int -> Int -> String
tabeladeFatoriais m n
    |m > n     = ""
    |otherwise = show m ++ " -> " ++ show (fatorial m) ++ "\n"
                  ++ tabeladeFatoriais (m + 1) n

fatorial :: Int -> Int
fatorial 0 = 1 
fatorial n = n * fatorial (n-1)


--questao 2 
linha :: Int -> String
linha x
    |x < 0     = show x ++ " -> indefinido\n"
    |otherwise = show x ++ " -> " ++ show (fatorial x) ++ "\n"


--verificador de par ou impar
ehImpar :: Int -> Bool
ehImpar 0 = False
ehImpar n = ehPar(n-1)

ehPar :: Int -> Bool
ehPar 0 = True
ehPar n = ehImpar (n-1)


--somar os quadrados usando where
somaQuadrados :: Int -> Int -> Int 
somaQuadrados n m = quadN + quadM 
  where
    quadN = n * n
    quadM = m*m

--qual dos quadrados é maior
maximoQuadrado x y
  |quadX > quadY = quadX 
  |otherwise = quadY
                where
                  quadX = quad x
                  quadY = quad y
                  quad :: Int -> Int
                  quad n = n*n 



--maximo de ocorrencias
maximasOcorrencias :: Int -> Int -> Int -> (Int, Int)
maximasOcorrencias n m p = (maior, qtd)
  where
    maior = max (max n m) p
    qtd = (if n == maior then 1 else 0) + (if m == maior then 1 else 0) + (if p == maior then 1 else 0)



--equação do segundo grau(solucaodolivro)
umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = -b / (2 * a)

duasRaizes:: Float -> Float -> Float -> (Float, Float)
duasRaizes a b c = (d+e, d-e)
  where 
    d = -b / (2*a)
    e = sqrt (b ^2 -4 * a * c) / (2*a)

saida :: Float -> Float -> Float -> String
saida a b c = cabecalho a b c ++ raizes a b c 

cabecalho :: Float -> Float -> Float -> String 
cabecalho a b c = "A equacao \n\n\t"++ show a ++ "*x^2"

raizes :: Float -> Float -> Float -> String
raizes a b c
  | b^2 > 4.0 * a * c = "duas raizes reais e distintas: "
  ++ show f ++ " e " ++ show s
  |b^2 == 4.0 * a * c = "duas raizes reais e iguais: "
  ++ show (umaRaiz a b c)
  |otherwise = "nenhuma raiz real "
    where (f, s) = duasRaizes a b c



{-
 Exercı́cios:
Para os exercı́cios a seguir, considere os pontos do plano como sendo do tipo Ponto =
(Float, Float). As linhas do plano são definidas por seus pontos inicial e final e têm o tipo
Linha = (Ponto, Ponto).
1. Defina funções que retornem a ordenada e a abcissa de um ponto.
2. Defina uma função que retorne a norma de um vetor dado por suas coordenadas.
3. Se uma linha é determinada pelos pontos (x1 , y1 ) e (x2 , y2 ), sua equação é definida por
(y − y1 )/(x − x1 ) = (y2 − y1 )/(x2 − x1 ). Defina uma função do tipo valorY :: F loat− >
Linha− > F loat que retorna a ordenada y do ponto (x, y), sendo dados x e uma linha.
4. Dados dois vetores, u e v, determine se eles são, ou não, colineares. 
-}











{-
4.7
Exercı́cios propostos
1. Dado um número natural n > 0, n é dito perfeito se a soma de seus divisores, incluindo o
número 1, é igual ao próprio n. O primeiro número natural perfeito é o número 6, porque
6 = 1 + 2 + 3. Implemente em Haskell uma função que informe se n é, ou não, um número
perfeito.
2. Dado um número natural n > 0, implemente em Haskell uma função que informe se n é,
ou não, um número primo.
3. Dê uma definição em Haskell da função f at que calcula o fatorial de n, onde n é um inteiro
positivo.
4. Dê uma definição em Haskell de uma função de m e n que retorna o produto m ∗ (m + 1) ∗
. . . ∗ (n − 1) ∗ n, para m < n e retorne 0 se m ≥ 0.
5. Dê uma definição em Haskell de uma função que retorne i -ésimo número da sequência de
Fibonacci (0, 1, 1, 2...).
6. Implemente, em Haskell, uma função que calcule o resto da divisão de dois números inteiros
positivos.
7. Implemente, em Haskell, uma função que calcule a divisão inteira entre dois números
inteiros positivos.
838. Implemente, em Haskell, uma função que calcule o máximo divisor comum entre dois
números inteiros positivos.
9. Implemente, em Haskell, uma função que calcule o mı́nimo múltiplo comum entre dois
números inteiros positivos.
-}

numeroPerfeito :: Int -> Bool
numeroPerfeito n = somaDivisores n (n-1) == n

somaDivisores :: Int ->Int -> Int 
somaDivisores n x
  |x == 1 = 1
  |mod n x == 0 = x + somaDivisores n (x-1)
  |otherwise = somaDivisores n (x-1)


--Questao 2
ehPrimo :: Int -> Bool
ehPrimo n = mod n 2 == 0

--questao 3 
fat :: Int -> Int
fat 0 = 1
fat n = n * fat(n-1) 

--questao 5
enesimoFib :: Int -> Int
enesimoFib 0 = 1
enesimoFib 1 = 1
enesimoFib 2 = 1
enesimoFib n = enesimoFib (n-1) + enesimoFib (n-2)

--questao 6 
restoDiv:: Int -> Int -> Int
restoDiv _ 0 = error "erro divisao por 0"
restoDiv x y 
  |x < y = x
  |x == y = 0
  |otherwise = restoDiv (x-y) y

--questao 7
divInteira :: Int -> Int -> Int
divInteira _ 0 = error "divisao por zero"
divInteira x y 
  |x < y = 0
  |x == y = 1
  |otherwise = 1 + divInteira (x - y) y
