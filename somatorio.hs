maior2 :: Int->Int->Int
maior2 n1 n2
 | n1 > n2 = n1
 | otherwise = n2

maior3 :: Int->Int->Int->Int
maior3 n1 n2 n3
 | (n1 > n2) && (n2> n3) = n1
 | (n2 >n3) = n3
 | otherwise = n3

m3 :: Int->Int->Int->Int
m3 n1 n2 n3 = maior2 (maior2 n1 n2) n3

som :: Int->Int
som 1 = 1
som n = som(n -1) + n

-- lista 2
--questão 1

intervalo :: Int->Int->List
intervalo a b
 | a>b = []
