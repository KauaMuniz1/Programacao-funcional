import Control.Concurrent (Chan)
import Control.Monad.Cont (label)
import GHC.Float (fromRat'')
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
--enesimoElem :: Int -> [Char]-> Char
--enesimoElem i l










--Escreva uma função que elimina caracteres repetidos. Eles devem
--ser substituídos por uma única cópia do elemento e a ordem dos
--elementos não deve ser alterada.
eliminaRep :: [Char] -> [Char]
eliminaRep [] = []
eliminaRep [cab] = [cab]
eliminaRep (cab: cauda)
   |cab /= head cauda = cab : eliminaRep cauda
   |otherwise = eliminaRep cauda





   





