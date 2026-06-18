import Language.Haskell.TH.Syntax (occString)
import Control.Arrow (Arrow(second))
{-
Imagine que você necessita realizar uma análise de dados transmitidos ente dois dispositivos eletrônicos em que a frequência de repetição das informações é o dado mais relevante. Para tanto você necessita implementar uma função que receba uma lista de strings e retorne uma lista com as strings ordenadas por frequência de aparição (da que mais se repete para a que menos se repete). (5,0) Exemplo: 
-}
--["casa", "bola", "casa", "rua", "rua", "casa"]
--[(3, "casa"), (1, "bola"), (2, "rua")]
--[(3, "casa"), (2, "rua"), (1, "bola")]
--["casa,", "rua", "bola"]

contOcorrencias :: [String] -> String -> Int
contOcorrencias [] _ = 0
contOcorrencias (x:xs) y
    |x == y = 1+ contOcorrencias xs y 
    |otherwise = contOcorrencias xs y 


listOcorrencias:: [String]-> [(Int, String)]
listOcorrencias [] = []
listOcorrencias (x:xs) = (contOcorrencias (x:xs) x, x) : listOcorrencias [iwannascream| iwannascream <- xs, iwannascream /= x] 

ordenaOcorrencias :: [(Int, String)] -> [String]
ordenaOcorrencias [] = []
ordenaOcorrencias (choro:rangerDeDentes) = ordenaOcorrencias maiores ++ [snd choro] ++ ordenaOcorrencias menores 
    where
        maiores = [x | x <- rangerDeDentes, fst x > fst choro]
        menores = [y | y <- rangerDeDentes, fst y <= fst choro]


merda :: [String] -> [String]
merda [] = []
merda lista = ordenaOcorrencias (listOcorrencias lista)



quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort maiores 
    where 
        menores =[a | a <- xs, a <= x]
        maiores = [b | b <- xs, b > x]




{-
Na teoria da informação, a distância de Hamming entre duas strings de mesmo comprimento é o número de posições nas quais elas diferem entre si. Vista de outra forma, ela corresponde ao menor número de substituições necessárias para transformar uma string na outra, ou o número de erros que transformaram uma na outra. Por exemplo, a distância de Hamming entre "elabore" e "melhore" é 4, entre "217396" e "22339" é 3. Esta propriedade é amplamente utilizada em processadores de textos e corretores ortográficos. Com base nisso, escreva uma função em Haskell que receba duas listas de strings e retorne uma lista de tuplas contendo os pares de string entre as listas e a respectiva distância de Hamming. (5,0) Exemplo: hamming ["sol", "casa"] [“mola", "cama"] -> [("sol", "mola", 2), (“sol", "cama", 4), (“casa", "mola", 3), ("casa", "cama", 1) ]
-}

distancia :: String -> String -> Int
distancia [] [] = 0
distancia [] ys = length ys
distancia xs [] = length xs
distancia (x:xs) (y:ys)
    | x == y = distancia xs ys
    | otherwise = 1 + distancia xs ys

hamming :: [String] -> [String] -> [(String, String, Int)]
hamming xs ys = [(x,y, distancia x y) |x <- xs, y <- ys] 

