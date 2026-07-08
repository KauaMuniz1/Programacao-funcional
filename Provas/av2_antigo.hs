totalFiltro :: (a -> Bool) -> [a] -> Int 
totalFiltro func lista = length (filter func lista)



--q2
repete :: (a,   Int )-> [a] 
repete (_,0) = []
repete(c,f) = [c] ++ (repete (c, f-1))

descompacta :: [(a, Int)] -> [a]
descompacta l = foldr1 (++) (map repete l)