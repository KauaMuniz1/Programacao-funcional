import Control.Concurrent (Chan)
import Control.Monad.Cont (label)
import GHC.Float (fromRat'')
import Distribution.TestSuite (Result(Error))
import GHC.IO.Device (IODevice(dup))
import Data.Char (isAlpha)
import Data.Time.Format.ISO8601 (yearFormat)
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
nEsimoElemento (x:xs) y
   |y == 0 = xs
   |otherwise = x : nEsimoElemento xs (y-1)



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
primeiraPalavra x = takeWhile isAlpha x



{-
Implemente uma função que move todos os elementos de uma
lista para a direita.Exemplo:
moverDireita [ 'a', 'b', 'c' ] 0 -> [ 'a', 'b', 'c' ]
moverDireita [ 'a', 'b', 'c' ] 1 -> [ 'c', 'a', 'b' ]
moverDireita [ 'a', 'b', 'c' ] 2 -> ['b', 'c', 'a']
-}

moverDireita :: String -> Int -> String
moverDireita (x:xs) 0 = (x:xs)
moverDireita (x:xs) y = moverDireita ((xs)++[x]) (y-1)


