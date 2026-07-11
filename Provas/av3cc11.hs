--minha versao

--q1 (errada)
reducaoCondicional :: [(t) -> u] -> (t -> Bool) -> [t] -> u 
reducaoCondicional f1 f2 l = f1 (filter f2 l)


--q2 
substituir :: [(t, u)] -> [t] -> [u]

substituir listaTupla [] = []

substituir listaTupla (z:zs) =
    aux listaTupla z : substituir listaTupla zs

aux :: [(t, u)] -> t -> u
aux [] _ = error "elemento não existente na lista"

aux ((x, y):xs) z
    | z == x    = y
    | otherwise = aux xs z



--Correcao do professor
--q1

reducaoCondicionalLF :: (t->t->t)->(t->Bool)->[t]->t
reducaoCondicionalLF func pred lista=foldr1 func (filter pred lista)

--q2
substituirLF::(Eq t)=>[(t,u)]->[t]->[u]
substituirLF dic lista = [snd tupla|e<-lista, tupla<-dic, e == fst tupla]