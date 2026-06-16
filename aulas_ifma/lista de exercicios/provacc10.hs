

ordenaFreq:: [String] -> [String]






conta::[String] -> String -> Int
conta []_ = 0
conta (a:xs) p
    |p == a + 1 + conta xs p
    |otherwise = conta xs p

contaTupla:: [String] -> String -> (Int, String)
contaTupls

--["casa", "bola", "casa", "rua", "rua", "casa"]
--[(3, "casa"), (1, "bola"), (2, "rua")]
--[(3, "casa"), (2, "rua"), (1, "bola")]
--["casa,", "rua", "bola"]