{-
Escreva a uma função polimórfica chamada aplicacaoExclusiva que receba como parâmetro duas
funções e duas listas polimórficas. A primeira função (de retorno booleano) deve validar quais
elementos da primeira e segunda listas são elegíveis para serem aplicados à segunda função.
Logo, o resultado final deve ser uma lista com o retorno da aplicação da segunda função a cada
um dos elementos selecionados pela primeira.
Exemplo:
aplicacaoExclusiva (>2) (+) [1,2,3,4] [9,5,0] = [12,8,13,9]
-}

--q1
aplicacaoExclusiva :: (t -> Bool) -> (t -> t -> u) -> [t] -> [t] -> [u] 
aplicacaoExclusiva f1 f2 l1 l2 = [ f2   e1 e2 |e1 <- (filter f1 l1), e2 <-(filter f1 l2)] 



--q2 
contaVizinhosIguais :: (Eq t) => [t] -> Int 
contaVizinhosIguais [] = 0
contaVizinhosIguais (a:xs)
    |xs == [] = 0
    |a == head xs = 1 + contaVizinhosIguais xs
    |otherwise = contaVizinhosIguais xs    

--q3 
contador :: (t -> Bool) -> [t] -> Int 
contador f l = length (filter f l)

--q4       
filtraPar :: (t -> Bool) -> (u -> Bool) -> [t] -> [u] -> [(t,u)]
filtraPar f1 f2 l1 l2 = [ (e1,e2)|e1 <- filter f1 l1, e2 <- filter f2 l2]



{-
Escreva a uma função polimórfica chamada selecionaExecuta que receba como parâmetro duas
funções e uma lista de um tipo qualquer. A primeira função deve validar quais elementos da lista
são elegíveis para aplicar a segunda função, sendo que o resultado final deve ser uma lista com o
retorno da segunda função aplicado a estes elementos. Portanto, a primeira função deve receber
um elemento da lista e retornar um valor booleano, já a segunda função deve receber um elemento
da lista e retornar outro elemento não necessariamente do mesmo tipo.
Exemplo:
selecionaExecuta (>2) (*3) [1,2,3,4,5] = [9,12,15]
selecionaExecuta isLower toUpper "aBcDfgGHi" = “ACFGI"
-}
--q5
selecionaExecuta :: (t -> Bool) ->  (t-> u) -> [t] -> [u] 
selecionaExecuta f1 f2 l = [f2 e1 | e1 <- filter f1 l]

--versao do LF 
selecionaExecutaLF :: (t -> Bool) -> (t -> u) -> [t] -> [u]
selecionaExecutaLF f1 f2 l = map f2 (filter f1 l) 