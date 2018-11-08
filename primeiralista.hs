import Data.Char (ord)

-- 1:
triplo :: Int -> Int
triplo n = 3*n

-- 2:
maior3 :: Int->Int->Int->Int
maior3 a b c
 | a>b && b>c = a
 | a<b && b>c = b
 | otherwise = c

-- 3:
somatorio :: Int->Int
somatorio 1 = 1
somatorio n = somatorio(n -1) + n

-- 4:
nEsimoTermoPA :: Int->Int->Int->Int
nEsimoTermoPA a1 r n = a1 + (n-1)*r 

-- 5:
nEsimoTermoPG :: Int->Int->Int->Int
nEsimoTermoPG a1 r n = a1*r^(n-1)

-- 6:
somaNPA :: (Int,Int,Int)->Int
somaNPA (a1, an,n) = sn
 where sn = div((a1 + an)*n) 2

--7:
somaPG :: (Int,Int,Int)->Int
somaPG(a1,q,n) = 
 let an = a1*q^(n-1)
 in an


--8: 
fib :: Int->Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

--9:
bissexto :: Int->String
bissexto ano
 | mod ano 400 == 0 = "bissexto"
 | mod ano 4 == 0 && mod ano 100 /= 0 = "bissexto"
 | otherwise = "nao bissexto"


--10:
fatores :: Int ->[Int]
fatores n = [i | i<-[1..n], n `mod` i == 0]
primo :: Int-> Bool
primo n = if (fatores n) == [1,n] then True
                                  else False

--11:

ehMaiuscula :: Char->Bool
ehMaiuscula c = if ord c >= 65 && ord c <= 90 then True else False 


--12:
ehMinuscula :: Char->Bool
ehMinuscula c = if ord c >= 97 && ord c <= 122 then True else False

--13:
isDigit :: Char->Bool
isDigit c = if ord c >= 48 && ord c <= 57 then True else False

--14:
repete :: String->Int->String
repete x n = take ns (cycle x)
 where ns = (length x) * n

--15:
nEspacos :: Int->String
nEspacos n = if n <=0 then "digite outro numero"
else take n (cycle " ")

--16:
lista3 :: Int->Int->Int->[Int]
lista3 a1 a2 a3 = [a1,a2,a3]

maior3' :: Int->Int->Int->(Int,Int)
maior3' a1 a2 a3 = (maior,menor)
 where maior = maximum (lista3 a1 a2 a3)
       menor = minimum (lista3 a1 a2 a3)

--17:
mdc:: (Int,Int)->Int
mdc (a,0) = a
mdc (a,b) = mdc(b, a `mod` b)

--18: 
maiorNatupla :: Int->Int->Int->(Int,Int,Int)
maiorNatupla a1 a2 a3 
 | a1 > a2 && a2 > a3 = (a1,a2,a3)
 | a3> a2 && a2 > a1 = (a3,a2,a1)
 | a2 >a3 && a3 > a1 = (a2,a3,a1)
 | a2 > a1 && a1 >a3 = (a2,a1,a3)
 | a1 >a3 && a3> a2 = (a1,a3,a2)
 | a3 > a1 && a1 > a2 = (a3,a1,a2)

--19:
type Ponto = (Float,Float)
type Reta = (Ponto,Ponto)
eqReta :: Ponto->Ponto->Reta
eqReta (x1,y1) (x2,y2) = (a,b)
                         where a = (x1-x2)
                               b = (y1-y2)
                               m = div y x 
--20
rVertical :: Reta->Bool
rVertical (p,q) = if fst p == fst q then True else False
rHorizontal :: Reta->Bool
rHorizontal (p,q) = if snd p == snd q then True else False


--21:
rIntersec :: Reta->Reta->Ponto


