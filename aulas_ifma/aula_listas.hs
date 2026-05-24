import Control.Concurrent (Chan)
--criar a função replicaChar
--recebe um char e um valor inteiro n
--cria uma lista de caracteres formado pelo caractere formado pelo


replicaChar :: Char -> Int -> [Char]
replicaChar c n
  |n==0 = []




--definir função inverte
--recebe uma ista de caracteres
--inverte a ordem dos elementos da lista

inverte :: [Char] -> [Char]
inverte [] = []
inverte (a: as) = inverte as ++ (a:[])



--ordenar lista de inteiros
--metodo: construir uma nova lista a partir da lista inicial, mas posicionando as cabeças corretamente

