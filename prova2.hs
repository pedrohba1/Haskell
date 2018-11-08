chain :: Integer -> [Integer]
chain 1 = [1]
chain n
 |even n = n : chain (n `div` 2)
 |odd n = n : chain (n*3 + 1) 

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs >15) (map chain [1..5]))	


--Testando algumas funções sobre cá9lculo lambda
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
somaTodos xs = foldl (\acc x -> acc + x) 0 xs

-- ou também:
-- somaTodos = foldl (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = (x==y) || elem x ys

--paa a prova : Saber criar um tipo algébrico, e carregar o tipo algébrico)
--saber diferenciar polimorfismo paramétrico de sobrecarregamento
-- saber trabalhar com expressões lambda em uma função











