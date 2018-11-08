fs :: Int->Int
fs s =  s +1

fx :: [Int]->[Int]
fx xs = [x *2| x<-xs]
maisn :: (Int->Int)->[Int]->[Int]
maisn fs [] =[]
maisn fs (x:xs) = (fs x):(maisn fs xs)

maisn' :: (a->a) ->[a]->[a]
maisn' fs xs = [fs x| x<-xs]


permuta :: [Int] -> Int -> [[Int]]
permuta _ 0 = []
permuta (x:xs) n = insere_comeco (permuta (x:xs) (n-1)) x

insere_comeco :: [[Int]] -> Int -> [[Int]]
insere_comeco [] _ = []
insere_comeco (x:xs) k = (k:x):(insere_comeco xs k)