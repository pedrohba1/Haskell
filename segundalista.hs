---
 
import Data.Set
import Control.Monad






--1:
intervalo_fechado :: Int -> Int -> [Int]
intervalo_fechado a b
    | a == b = [b]
    | a > b = []
    | otherwise = a:intervalo_fechado (a+1) b

--2:
repete2 :: [Int] -> [Int]
repete2 [] = []
repete2 (h:t) = h:h:repete2 t

--3:
divisores_de :: Int -> [Int]
divisores_de n = [x |x<-[1..n], mod n x == 0]

primos :: [Int] -> [Int]
primo [] = []
primos (h:t) 
    |h == 1 = []
    |divisores_de h == [1,h] = (h:primos t)
    |otherwise = primos t

 --4, 5 e 6:
    
insereOrd :: (Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (h:t)
    |x <= h = (x:h:t)
    |otherwise = (h:insereOrd x t)

    

ordenaLista :: (Ord a) => [a] -> [a]
ordenaLista [] = []
ordenaLista (h:t) = insereOrd h (ordenaLista t)
    

insereordTupla :: (Int,String)->[(Int,String)]->[(Int,String)]
insereordTupla x [] = [x]
insereordTupla (x,y) ((h,t1) : t)
 |x <= h = (x,y):(h,t1):t
 |otherwise = (h,t1):(insereordTupla (x,y) t)

    --retorna os pares de uma lista
pares :: [Int] -> [Int]
pares (h:t)
    |t == [] = []
    |even h = (h:pares t)
    |otherwise = pares t
    
    --retorna os impares de uma lista
impares :: [Int] -> [Int]
impares (h:t) 
    |t == [] = []
    |odd h = (h:impares t)
    |otherwise = impares t

    --funcao que separa os pares e os impares de uma lista
separaLista :: [Int] -> [Int]
separaLista l = pares l ++ impares l

--7)
somaConjuntos :: [Int] -> [Int] -> [Int]
somaConjuntos l1 l2 = ordenaLista (l1 ++ l2)
 
--8:

combinacoesRepet ::[Char]->[[Char]]
combinacoesRepet list = []

removetodos :: Int->[Int]->[Int]
removetodos x f = filter (/= x) f



listaParaConjunto :: [Int] -> [Int]
listaParaConjunto [] = []
listaParaConjunto (h:t) =
    (h:(listaParaConjunto (removetodos h t)))

--11)
    --funcao pertence
pertence :: Int -> [Int] -> Bool
pertence x [] = False
pertence x (h:t) 
    |x == h = True
    |otherwise = pertence x t

    -- a)
uniao :: [Int] -> [Int] -> [Int]
uniao [] [] = []
uniao c1 c2 = listaParaConjunto (c1++c2)

-- b)
unir :: [Int] -> [Int] -> [Int]
unir [] l = l
unir (h:t) l
    |pertence h l = unir t l
    |otherwise = h:unir t l

--c)
une :: [Int] -> [Int] -> [Int]  
une x y = x ++ [z | z <- y , not (pertence z x )]


--12:
--a)
multiplos3 :: [Int]
multiplos3 = [x | x <-[102,105..300]]




--8>
powerset :: Ord a => Set a -> Set (Set a)
powerset [] = [[]]
powerset (head:tail) = acc ++ map (head:) acc where acc = powerset tail






