--função que verifica se um determinado número inteiro pertence, ou não, a uma lista de inteiros.

pertence :: Int -> [Int] -> Bool
pertence b [] = False 
pertence b (a:x) = (b == a) || pertence b x 


--funcao para dobrar os elementos de uma lista

dobra :: [Int] -> [Int]
dobra [] = []
dobra (x:xs) = (x*2) : dobra xs

--usando zf
dobraZF :: [Int] -> [Int] 
dobraZF [] = []
dobraZF lista = [x * 2| x <- lista]


--Defina a função product :: [Int] -> Int que retorna o produto de uma lista de inteiros.
productfunc :: [Int] -> Int 
productfunc [] = 1
productfunc (x:xs) = x * productfunc xs


{-
Defina a função concat :: [[Int]] -> [Int] que concatena uma lista de listas de in-
teiros transformando-a em uma lista de inteiros. Por exemplo, concat[[3, 4], [2], [4, 10]] =
[3, 4, 2, 4, 10].
-}
concatFunc :: [[Int]] -> [Int]
concatFunc [] = []
concatFunc (x:xs) = x ++ concatFunc xs

--usando ZF 
concatFuncZF :: [[Int]] -> [Int]
concatFuncZF [] = []
concatFuncZF lista = [x | subLista <- lista, x <- subLista]