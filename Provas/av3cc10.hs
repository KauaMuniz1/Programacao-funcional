import Data.Char

--q1 
filtroExecucao :: (t -> u) -> (u -> Bool) -> [t] -> [u] 
filtroExecucao f1 f2 lista = filter f2 (map f1 lista)


--q2 
contaRepeticao :: (Eq t) => [t] -> Int 
contaRepeticao [] = 0
contaRepeticao (a:xs)
    |xs == [] = 1
    |a == head xs = 1 + contaRepeticao xs
    |otherwise = 1 

listaRepeticao :: Eq t => [t] -> [([t], Int)]
listaRepeticao [] = []
listaRepeticao l = [([head l], cont)] ++ listaRepeticao (drop cont l )
    where 
        cont = contaRepeticao l

compressaoRepeticao :: Eq t => ([t], Int) -> [([t], Int)] -> [([t], Int)]
compressaoRepeticao (a,b) [] = [(a,b)]

compressaoRepeticao (a,b) ((c,d):ys) | b /= d = (a,b) : compressaoRepeticao (c,d) ys 
                                     | b == 1 = compressaoRepeticao (a ++ c, b) ys
                                     | otherwise = (a,b) : compressaoRepeticao (c,d) ys

