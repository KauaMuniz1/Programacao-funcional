import Control.Concurrent (Chan)
import Control.Monad.Cont (label, cont)
import GHC.Float (fromRat'')
import Distribution.TestSuite (Result(Error), Test (Test))
import GHC.IO.Device (IODevice(dup))
import Data.Char (isAlpha)
import Data.Time.Format.ISO8601 (yearFormat)
import Control.Applicative (Alternative(some))
--funcao que retorna se um numero pertence ou nao a uma lista

pertenceLista :: [Int] -> Int -> Bool
pertenceLista [] _= False
pertenceLista (x:xs) n
   |x == n    = True 
   |otherwise = pertenceLista xs n



--Escreva uma função que retorne o maior elemento de uma lista de inteiros

maiorElemento :: [Int] -> Int
maiorElemento[x] = x
maiorElemento (x:xs)
    |x > maiorElemento xs = x
    |otherwise = maiorElemento xs



--Escreva uma função que receba um número inteiro positivo n,
--uma lista e retorne o n-ésimo elemento da lista.
enesimoInt :: Int -> [Int] -> Int
enesimoInt _ [] = error "lista vazia"
enesimoInt y (x : xs)
   |y == 0 = x
   |otherwise = enesimoInt (y-1) (xs)



{-
Escreva uma função que retire o n-ésimo elemento de uma lista.
Exemplo:
"abcdefghi" 3 -> "abdefghi"
-}
nEsimoElemento :: String -> Int -> String
nEsimoElemento [] _ = error "palavra vazia"
nEsimoElemento s 0 = s
nEsimoElemento (x:xs) y
   |y == 1 = xs
   |otherwise = x: nEsimoElemento xs (y-1)



{-
Escreva uma função que receba uma frase e diga se esta é um
palíndromo. Para veriﬁcar se uma frase é palíndromo basta
veriﬁcar se ela é igual à sua reversa (implemente a função
reverse).
-}
reverseFrase :: String -> String
reverseFrase [] = [] 
reverseFrase (x:xs) = reverseFrase (xs) ++ [x]


palindromo :: String -> Bool
palindromo [] = error "lista vazia"
palindromo (x : xs)
   |reverseFrase(x:xs) == (x : xs) = True
   |otherwise = False

--Escreva uma função que elimina caracteres repetidos. Eles devem
--ser substituídos por uma única cópia do elemento e a ordem dos
--elementos não deve ser alterada.
eliminaRep :: [Char] -> [Char]
eliminaRep [] = []
eliminaRep [cab] = [cab]
eliminaRep (cab: cauda)
   |cab /= head cauda = cab : eliminaRep cauda
   |otherwise = eliminaRep cauda



{-
Escreva uma função que duplique cada elemento de uma lista.
Exemplo:
[1, 2, 3] -> [1,1,2,2,3,3]
-}

duplicaElem:: [Int] -> [Int]
duplicaElem [] = []
duplicaElem (x : xs) = 2 * x : duplicaElem xs



{-
Escreva uma função que recebe uma String e retorna a primeira
palavra dessa String sem contar pontuação.
Exemplo:
“Olá, mundo” -> “Olá”
-}
primeiraPalavra :: String -> String
primeiraPalavra [] = []
primeiraPalavra (a:x)
   | a == ' ' || a == '!' || a == '?' || a == ',' || a == '.' || a == ';' = []
   | otherwise = a : primeiraPalavra x


{-
Implemente uma função que move todos os elementos de uma
lista para a direita.Exemplo:
moverDireita [ 'a', 'b', 'c' ] 0 -> [ 'a', 'b', 'c' ]
moverDireita [ 'a', 'b', 'c' ] 1 -> [ 'c', 'a', 'b' ]
moverDireita [ 'a', 'b', 'c' ] 2 -> ['b', 'c', 'a']
-}

moverDireita :: String -> Int -> String
moverDireita s n = drop (tam-n) s ++ take (tam-n) s 
   where
      tam = length s


{- Implemente uma função que recebe duas listas sem elementos
repetidos e retorna uma lista com elementos comuns entre elas.
Exemplo:
intercede [1,2,3,4] [2,3,4,5] -> [2,3,4]Implemente uma função que recebe duas listas sem elementos
repetidos e retorna uma lista com elementos comuns entre elas.
Exemplo:
intercede [1,2,3,4] [2,3,4,5] -> [2,3,4]

-}
intercede :: [Int] -> [Int] -> [Int]
intercede [] _ = []
intercede _ [] = []
intercede (x:xs) (y:ys) 
   |pertenceIntercede x (y:ys) == True = x : intercede xs (y:ys)
   |otherwise = intercede xs (y:ys)
   

pertenceIntercede :: Int -> [Int] -> Bool 
pertenceIntercede _ [] = False 
pertenceIntercede n (x:xs)
   |n == x = True
   |otherwise = pertenceIntercede n (xs)


--usando zf
intercedeZF :: [Int] -> [Int] -> [Int]
intercedeZF []_ = []
intercedeZF _[] = []
intercedeZF lista1 lista2 = [(x) | x <- lista1, y <- lista2, x == y ]






{-11. Implemente a função 'split', que recebe um número inteiro n e uma
lista de números inteiros e retorna uma tupla onde o primeiro
elemento é uma lista dos itens maiores que n e o segundo
elemento é uma lista dos itens menores.
Exemplo:
split 5 -> [1,2,3,4,5,6,7,8] -> ([6,7,8],[1,2,3,4,5])
-}

split :: Int -> [Int] -> ([Int], [Int])
split n s = (drop n s, take n s)

--versao com recursao

splitRecursao :: Int -> [Int] -> ([Int], [Int])
splitRecursao _ [] = ([],[])
splitRecursao 0 l = (l, [])
splitRecursao n (a:x) = (prim, a:seg)
   where
      (prim,seg) = split(n-1) x




{-
12. Escreva uma função que dados dois índices, m e n, extraia da
lista os elementos compreendidos entre entre esses valores, onde
ambos os limites estão incluídos. Comece a contar os elementos
do 1.
Exemplo:
['a','b','c','d','e','f','g','h','i','k'] 3 7 -> "cdefg"
-}

selectElementos :: [Char] -> Int -> Int-> [Char]
selectElementos [] _ _ = []
selectElementos (x:xs) y z 
   |z < y = error "intervalo negativo"
   |y > 1 = selectElementos(xs) (y-1) (z-1)
   |y == 1 && z > 1 =  x : selectElementos(xs) y (z-1)
   |y == z = [x]


--usando ZF
selectElementosZF :: [Char] -> Int -> Int -> [Char]
selectElementosZF [] _ _ = []
selectElementosZF lista y z = [letra| (pos,letra) <- zip [1..]lista, pos >= y, pos <= z]




{-
13. Escreva uma função que empacote duplicatas consecutivas de
elementos de lista em sublistas. Se uma lista contém elementos
repetidos, eles devem ser colocados em sublistas separadas.
Exemplo:
['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] -> ["aaaa","b","cc","aa","d","eeee"]
-}
subListDuplicatas :: [Char] -> [Char] 
subListDuplicatas [] = []
subListDuplicatas (x:xs) = (x:contVezesList xs x) ++ subListDuplicatas (drop (length (contVezesList xs x)) xs)

contVezesList :: [Char] -> Char -> [Char]
contVezesList [] _ = []
contVezesList (x:xs) y
   | y == x    = x : contVezesList xs y
   | otherwise = [] 
--nao consegui fazer a questao
















{-
21. O índice de massa corporal (IMC) é uma medida simples para
classiﬁcar o peso de adultos. O IMC de um indivíduo é calculado
como o valor do peso (em quilogramas) a dividir pelo quadrado
da altura (em metros):
IMC = /²
Por exemplo: um indivíduo com 70 Kg e 1.70m de altura tem IMC
igual a 70/1. 70² ≈ 24. 22.
Classiﬁcamos o resultado nos seguinte intervalos:
● IMC < 18.5 baixo peso
● 18.5 ≤ IMC < 25 peso normal
● 25 ≤ IMC < 30 excesso de peso
● 30 ≤ IMC obesidade
Escreva uma deﬁnição da função
classiﬁca :: [(Int, Float, Float)] -> [(Int, String)]
que determina a classiﬁcação acima para uma lista composta
por um ID, o peso em quilogramas e a altura em metros e produz
como saída a uma lista contendo o ID e sua respectiva
classiﬁcação.
-}

classifica :: [(Int, Float, Float)] -> [(Int, String)]
classifica [] = []
classifica ((id, peso, altura): xs) 
   |imc >= 30 = (id, "Obesidade"): classifica xs
   |imc <30 && imc >= 25 = (id, "Sobrepeso"): classifica xs
   |imc < 25 && imc >= 18.5 = (id, "peso normal"): classifica xs
   |otherwise = (id,"peso baixo"): classifica xs
      where 
         imc = peso /(altura ^ 2)




{-
22. Deﬁna uma função
divprop ∶∶ Integer → [Integer]
usando uma lista em compreensão para calcular a lista de
divisores próprios de um inteiro positivo (i.e. inferiores ao número
dado).
Exemplo:
divprop 10 = [1, 2, 5].
-}

divprop :: Integer -> [Integer]
divprop x = auxDivProp x 1

auxDivProp :: Integer -> Integer -> [Integer]
auxDivProp x y 
   |x == y = []
   |mod x y == 0 = y : auxDivProp x (y+1)
   |otherwise = auxDivProp x (y+1)


--usandoZF 
divpropZF :: Integer -> [Integer]
divpropZF 1 = [1]
divpropZF a = [x | x <- [1..a-1], mod a x == 0]

{-
25.A cifra de César é um dos métodos mais simples para codiﬁcar
um texto: cada letra é substituída pela que dista k posições à
frente no alfabeto; se ultrapassar a letra Z, volta à letra A. Por
exemplo, para k = 3, a substituição efetuada é
ABCDEFGHIJKLMNOPQRSTUVWXYZ
DEFGHIJKLMNOPQRSTUVWXYZABC
Por exemplo, “ATAQUE DE MADRUGADA” é transformado em
“DWDTXH GH PDGUXJDGD”.
Escreva uma função
cifrar ∶∶ Int → String → String
para cifrar uma cadeia de caracteres usando um deslocamento
dado. Note que cifrar (−n) é a função inversa de cifrar n, pelo que
a mesma função pode servir para codiﬁcar e decodiﬁcar.
-}

alfabeto = ['a'..'z']

desloca :: Char -> Int -> String -> Char
desloca c 0 _ = c
desloca c n [] = desloca c n alfabeto
desloca c n (a:x)
   |c /= a = desloca c n x 
   |n > length (a:x) = desloca c (mod n (length (a:x))) (a:x)
   |otherwise = desloca (head x) (n-1) (x) 






{-
23.Um inteiro positivo n diz-se perfeito se for igual à soma dos seus
divisores (excluindo o próprio n). Defina uma função
perfeitos ∶∶ Integer → [Integer]

que calcula a lista de todos os números perfeitos até um limite
dado como argumento.
Exemplo:
perfeitos 500 -> [6, 28, 496].
Sugestão: utilize a solução do exercício anterior.
-}


perfeitos :: Int ->[Int]
perfeitos n 
   |n == 0 = []
   |ehPerfeito(n) == True = [n] ++ perfeitos(n-1)
   |otherwise = perfeitos(n-1)

ehPerfeito :: Int -> Bool
ehPerfeito 1 = False
ehPerfeito n = somaDivisores n (n-1) == n 

somaDivisores :: Int -> Int -> Int
somaDivisores n m |m == 1 = 1
                  |mod n m == 0 = m + somaDivisores n (m-1)
                  |otherwise = somaDivisores n (m-1)

{-
29. Escreva uma definição da função

toBits ∶∶ Int → [Int]

que obtém a representação em binário de um inteiro
não-negativo.
Exemplo:
toBits 29 = [1, 1, 1, 0, 1].
Note que os dígitos binários do resultado estão pela ordem do
mais significativo para o menos significativo.
-}

toBits :: Int -> [Int]
toBits x
   |x < 0 = []
   |x == 0 = [0]
   |x == 1 = [1]
   |x `mod` 2 == 0 = toBits(x `div` 2) ++ [0]
   |otherwise = toBits(x `div` 2) ++ [1]


{-
30.Escreva uma definição função da função
fromBits ∶∶ [Int] → Int

que faz a transformação inversa da anterior, ou seja, converte
dígitos em binário para o inteiro não-negativo correspondente.
Exemplo:
fomBits [1, 1, 1, 0, 1] = 29.
-}

fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs) 
   |x /= 0 && x /= 1 = error "numero nao binario"
   | otherwise = x * (2 ^ (length(x:xs)-1)) + fromBits(xs)










