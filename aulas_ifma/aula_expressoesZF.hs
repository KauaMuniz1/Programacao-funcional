import Control.Concurrent (Chan)
import Control.Monad.Cont (label)
import GHC.Float (fromRat'')
import Distribution.TestSuite (Result(Error))
import GHC.IO.Device (IODevice(dup))
import Data.Char (isAlpha)
import Data.Time.Format.ISO8601 (yearFormat)
import Control.Applicative (Alternative(some))


listaDivisores :: Int ->  Int -> [Int]
listaDivisores n d 
    |d == 1 = [1]
    |mod n d == 0   = listaDivisores n (d-1) ++ [d]
    |otherwise = listaDivisores n (d-1)
    

divisores :: Int -> [Int]
divisores n =  [x| x <- [1..(n-1)], mod n x == 0]

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1.. n], sum (divisores x) == x]


concatena :: [[Int]] -> [Int] 
concatena c = [y | x <- c, y <- x]


