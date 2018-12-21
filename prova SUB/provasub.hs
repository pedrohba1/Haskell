import Data.Char 

--função que recebe uma lista e retorna uma lista de caracteres:
retornalista :: String -> [String]
retornalista [] = []
retornalista l = map (:[]) l

edigito :: [String] -> [[Bool]]
edigito  x = map (map isDigit) x

maisc :: [String] -> [String]
maisc [] = []
maisc(l:ls) = (map (toUpper) l):(maisc ls)

totalchar :: String-> ([String], Int)
totalchar s = ((retornalista s), length s)
--          esse (a->b->c) é uma função.
--          ou seja. a primeira entrada é uma função que recebe
--			'a' e 'b' como argumentos e retorna c como resultado.
zipWith' :: (a->b->c) -> [a]->[b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip' :: (a->b->c) ->(b->a->c)
flip' f = g
 where g x y = f y x 

map' :: (a->b)-> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x :  map' f  xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
     let smallerOrequal = filter (<=x) xs
         larger = filter  (>x) xs
     in quicksort smallerOrequal ++ [x] ++ quicksort larger 


-- sequencia de collatz
chain :: Integer -> [Integer]
chain 1  = [1]
chain n
 | even n = n: chain ( n `div` 2)
 | odd n = n: chain (3 *n + 1)

numLong :: Int
numLong = length ( filter isLong (map chain [1..100]))
 where isLong xs = length xs > 15

--função "sum" 
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc +x) 0 xs

--aqui vão várias funções que usam fold pra fazer as coisas:
--função map só que começando pela direita:
mapr :: (a->b) -> [a]-> [b]
mapr f xs = foldr (\x acc -> f x : acc )  [] xs

--implementando a função map usando o foldl
mapl :: ( a -> b ) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

separa :: String -> [(Char, Int)]
separa [] = []
separa (x:xs) = (x, tam+1): separa ((drop tam) xs)
 where tam = length (takeWhile (==x) xs)