import Control.Concurrent (Chan)
import Control.Monad.Cont (label)
import GHC.Float (fromRat'')
import Distribution.TestSuite (Result(Error))
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
   |y == 1 && z > 1 =  head(x:xs) : selectElementos(xs) y (z-1)
   |y == z = [x]


--usando ZF
selectElementosZF :: [Char] -> Int -> Int -> [Char]
selectElementosZF [] _ _ = []
selectElementosZF lista y z = [letra| (pos,letra) <- zip [1..]lista, pos >= y, pos <= z]


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










