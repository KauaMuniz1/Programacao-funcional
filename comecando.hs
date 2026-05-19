import GHC.RTS.Flags (GCFlags(doIdleGC))
import Data.Text.Lazy.Read (double)
doubleMe x = x+x

doubleUS x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

