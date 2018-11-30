chain :: Integer -> [Integer]
chain 1 = [1]
chain n
 |even n = n : chain (n `div` 2)
 |odd n = n : chain (n*3 + 1) 

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs >15) (map chain [1..5]))	


--Testando algumas funções sobre cálculo lambda
zipar :: [Float]
zipar = zipWith (\a b -> (a*30 +3 ) / b) [1,2,3,4,5] [1,2,3,4,5]

usandotupla :: [Int]
usandotupla = map (\(a,b) -> a+b) [(1,2), (3,4)]

--escrevendo uma função em cálculo lambda
addThree :: Int-> Int-> Int-> Int
addThree  = \x -> \y -> \z -> x + y + z

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x
	
flipper :: [[Char]]
flipper = zipWith (flip (++)) ["te amo", "me ama"] ["eu ", "vc "]


--usando calculo lambda com o fold

somaTodos ::(Num a) => [a] -> a
somaTodos xs = foldl (\acc x -> acc +x) 0 xs

-- ou também:
-- somaTodos = foldl (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = (x==y) || elem x ys

--paa a prova : Saber criar um tipo algébrico, e carregar o tipo algébrico)
--saber diferenciar polimorfismo paramétrico de sobrecarregamento
-- saber trabalhar com expressões lambda em uma função


data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point 
 deriving (Show)


area :: Shape -> Float
area (Circle _ r) = 3.14* r^ 2
area (Rectangle  (Point x1 y1)  (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1) 



data LL = Latitude Int Int Int
 |Longitude Int Int Int deriving (Eq, Ord)
instance Show LL where	
 show (Latitude a b c) = "Lat " ++ show a ++"°" ++ show b ++"'"++ show c ++ "''"
 show (Longitude a b c) = "Long " ++ show a ++"°" ++ show b ++"'"++ show c ++ "''"
type PosicaoLocal = (String, LL, LL)
type Cidades = [PosicaoLocal]


c1,c2::PosicaoLocal
c1 = ("Brasilia", Latitude (-15) 46 47, Longitude 47 55 47)
c2 = ("Uberlandia", Latitude (-18) 55 07, Longitude 48 16 38)
equador = ("equador", Latitude 0 0 0, Longitude 0 0 0)
eLat::PosicaoLocal->(String,LL)
eLat (p,(Latitude a b c), (Longitude x y z)) = (p,(Latitude a b c)) 

eLong ::PosicaoLocal->(String,LL)
eLong (p, (Latitude a b c), (Longitude x y z)) = (p, (Longitude x y z))

norteDe :: PosicaoLocal->PosicaoLocal->Bool
norteDe ((p, Latitude a b c, Longitude x y z)) (p1, (Latitude a1 b1 c1), (Longitude x1 y1 z1))
 |a > a1 && b > b1 && c> b1 = True
 |otherwise = False

lcidades::Cidades
lcidades =
 [("Rio Branco", Latitude 09 58 29, Longitude 67 48 36),
 ("Brasilia", Latitude (-15) 46 47, Longitude 47 55 47),
 ("Torres", Latitude (-29) 20 07, Longitude 49 43 37),
 ("Joao Pessoa", Latitude (-07) 06 54, Longitude 34 51 47),
 ("Uberlandia", Latitude (-18) 55 07, Longitude 48 16 38)]

abaixoEq :: Cidades-> [String]
abaixoEq ((p, Latitude a b c, Longitude x y z):xs)
 |a <0 = [p] ++ (abaixoEq xs)
 |otherwise = abaixoEq xs

abaixoEq' :: Cidades -> Int
abaixoEq' [] = 0
abaixoEq' ((p, Latitude a b c, Longitude x y z):xs) 
 |a < 0 =  1+ abaixoEq' xs 
 |otherwise = abaixoEq' xs

entre :: Cidades-> [String]
entre [] = []
entre ((p, Latitude a b c, Longitude x y z):xs)
 |a >  40 && a< 50 = [p] ++ (entre xs)
 |otherwise = entre xs


data Talvez a = Valor a | Nada deriving (Show)
divisaoSegura :: Float -> Float -> Talvez Float
divisaoSegura x y = if y == 0 then Nada else Valor (x/y)	


addPares :: [(Int,Int)] -> [Int]
addPares lista =  [ m + n|(m,n) <- lista, m <n]


addPares2 :: (Integral a) => [(a,a)] -> [a]
addPares2 = map (\(x,y) -> x + y)


addParesT2 :: (Num a, Ord a) => [(a,a)] -> [a]
addParesT2 = \xs -> map(\(x,y) -> x+y) (filter (\(x,y) -> x<y) xs)


--questao 7
mp f [] ys = []
mp f xs [] = []
mp f (x:xs) (y:ys) = f x y : mp f xs ys



--questão 8:
somaQuad:: Int -> Int
somaQuad  n = foldr1 (+) (map (^2) [1..n])

--questão 9:
somaQuadPos :: ( Num a) => (Ord a) => [a] -> a
somaQuadPos xs = foldl (\acc x -> if x >0 then acc + x*x else acc) 0 xs



