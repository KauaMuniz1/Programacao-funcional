
venda :: Int -> Int
venda 0 = 7
venda 1 = 6
venda 2 = 4
venda 3 = 8
venda 4 = 2
venda 5 = 17
venda _ = 0


maiorSemanaVenda :: Int -> Int
maiorSemanaVenda n 
    |n == 0 = 0 
    |venda n >= venda(maiorSemanaVenda (n-1)) = n
    |otherwise = maiorSemanaVenda(n-1)

resultado = maiorSemanaVenda 2

semanaNadaVendido :: Int -> Bool
semanaNadaVendido n
    |venda n > 0 = True
    |otherwise = False



