import Control.Concurrent (Chan)
import Control.Monad.Cont (label, cont)
import GHC.Float (fromRat'')
import Distribution.TestSuite (Result(Error), Test (Test))
import GHC.IO.Device (IODevice(dup))
import Data.Char
import Data.Time.Format.ISO8601 (yearFormat)
import Control.Applicative (Alternative(some))
import Data.List

fazPar:: [t] -> [u] -> [(t,u)]
fazPar [] _ = []
fazPar _[] = []
fazPar lista1 lista2 = [(x,y) | x <- lista1, y <- lista2]

--funcao head
headd:: [t] -> t
headd (x:xs) = x 
 

taill:: [t] -> [t] 
taill (x:xs) = xs

fstt:: (t,u) -> (t)
fstt (x,xs) = x




--definir uma funcao concat onde concat [e1, ..., ek] = e1 ++ ... ++ek qual o tipo de concat 
--concatt :: [[t]] -> [t]
--concatt [] = []
--concatt lista = [x !! i|x <- lista , i <- [0.. length lista]] errado

concattZF :: [[t]] -> [t]
concattZF lista = [x| y <- lista, x <- y]

concatt :: [[t]] -> [t]
concatt [] = []
concatt (x:xs) = x ++ concatt xs



--defina uma função unzip que transforma uma lista de pares em um par de listas. qual o seu tipo?
unzippZF :: [(u,v)] -> ([u],[v])
unzippZF [] = ([],[])
unzippZF lista = ([x | (x,_) <- lista],[y| (_,y) <- lista])

--versao com recursao 
unzipp :: [(u,v)] -> ([u],[v])
unzipp [] = ([],[])
unzipp ((x,y):xs) = (x:as, y:bs)
    where 
        (as,bs) = unzipp xs