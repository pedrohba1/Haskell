--1:
intervalo_fechado :: Int -> Int -> [Int]
intervalo_fechado a b
    | a == b = [b]
    | a > b = []
    | otherwise = a:intervalo_fechado (a+1) b

--outra alternativa 
intervaloFechado :: Int->Int->[Int]
intervaloFechado a b = [x | x<- [a..b]]

--2:
repete2 :: [Int] -> [Int]
repete2 [] = []
repete2 (h:t) = h:h:repete2 t

--outraalternativa
repete22 :: [Int] -> [Int]
repete22 ls = concatMap (replicate 2) ls

--3:
divisores_de :: Int -> [Int]
divisores_de n = [x |x<-[1..n], mod n x == 0]


--4:
insere_numbr_ord :: Int->[Int]->[Int]
insere_numbr_ord x [] = [x]
insere_numbr_ord x (h:t)
 | x <= h = (x:h:t)
 | otherwise = (h:insere_numbr_ord x t)

ordena_numb :: [Int]->[Int]
ordena_numb [] = []
ordena_numb (h:t) = insere_numbr_ord h (ordena_numb t)

 --5:  
insereOrd :: (Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (h:t)
    |x <= h = (x:h:t)
    |otherwise = (h:insereOrd x t)

ordenaLista :: (Ord a) => [a] -> [a]
ordenaLista [] = []
ordenaLista (h:t) = insereOrd h (ordenaLista t)

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

--6:
separaLista :: [Int]->[[Int]]
separaLista l = [x,y]
 where x = pares l
       y = impares l
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
--a) retorna os multiplos de 3 entre 100 e 300
multiplos3 :: [Int]
multiplos3 = [x | x <-[102,105..300]]


    --b)
primos :: [Int] -> [Int]
primo [] = []
primos (h:t) 
    |h == 1 = []
    |divisores_de h == [1,h] = (h:primos t)
    |otherwise = primos t

--outra alternativa, com a lista [1..100]
primos2 :: [Int]
primos2  = [x | x<-[1..100], length (divisores_de x) <=2]  
    
--c)
blend :: [Int]->[Int]->[Int]
blend (x:xs) ys = x:(blend ys xs)
blend _ _ = []

parImpar :: [Int]
parImpar = blend a b 
 where a = pares (multiplos3)
       b = impares (primos2)

--d)
fracoes ::[String]
fracoes =[ show x ++ "/" ++ show y | x<-multiplos3, y <-primos2]

polinomio :: [Float]
polinomio = [ x ^2 + 7*x + 1| x<-[0.0,0.1..1.0]]

quatrobits :: [[Int]]
quatrobits = [ [a,b,c,d] | a<-[1,0], b<-[1,0], c<-[1,0], d<-[1,0]]












