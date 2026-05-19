
--mul :: Int->Int->Int
--mul m n
--    |m == 0 || n == 0 = 0
--    |otherwise = n + mul (m-1) n


mul:: Int-> Int-> Int
mul 0 _ = 0
mul n m = n + mul (m-1) n


main = putStrLn (show(mul 999999999999999999 9))
