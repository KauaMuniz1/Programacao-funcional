{-
contaAmigaveis:: [(Int, Int)] -> Int
contaAmigaveis [] = 0
contaAmigaveis Lista = length [(x,y) | a <- lista, x <- fst a, y <- snd a, ehAmigavel x y == True]

ehAmigavel :: Int -> Int -> Bool 
ehAmigavel x y 
    |somaDiv x 1 == y && somaDiv y 1 == x = True 
    |otherwise = False 

somaDiv :: Int -> Int -> Int 
somaDiv y == x = 0 
somaDiv x y 
    |mod x y == 0 = y + somaDiv x (y+1)
    |otherwise = somaDiv x (y+1)


seqGen :: [(Char, Int)] -> String 
seqGen [] = []
seqGen ((x,y):xs) = replicate y x ++ seqGen xs

-}

--correção do professor
--1 questao

repete :: Char -> Int -> String 
repete _ 0 = ""
repete c f = [c] ++ repete c (f-1)

descompacta :: [(Char, Int)] -> String 
descompacta [] = []
descompacta ((c, f) : cauda) = repete c f ++ descompacta cauda 


--questao 2
divisores :: Int -> [Int] 
divisores n = [d|d <- [1.. (n-1)],(mod n d == 0)]

amigaveis :: Int -> Int -> Bool 
amigaveis n1 n2 = (sum (divisores n1) == n2) && (sum(divisores n2) == n1)

contaAmigaveis :: [(Int, Int)] -> Int 
contaAmigaveis [] = 0
contaAmigaveis ((n1,n2) : cauda)
    |amigaveis n1 n2 = 1+ contaAmigaveis cauda
    |otherwise = contaAmigaveis cauda 

