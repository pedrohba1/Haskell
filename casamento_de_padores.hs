ifmport Prelude hiding ((||))
import Prelude hiding ((&&))

-- três definições para || à seguir

--1:
--(||) :: Bool->Bool->Bool
--True || True = True
--_ || _ = False


--2:
--(||) :: Bool->Bool->Bool
--True || b = b
--a || True = a
--False || False = Fals

--3:
--(||) :: Bool-> Bool -> Bool
--True || True = True
--False || False = False
--_ || _ = True


opp :: (Int ,(Int,Int)) -> Int
opp z = if fst z == 1
        then fst( snd z) + snd (snd z)
        else if fst z == 2
             then fst (snd z) -snd(snd z)
             else 0		



opp2 :: (Int, (Int, Int)) ->Int
opp2 (a, (b,c))
 | a == 1 = b+c
 | a == 2 = b-c
 
