

-- > (\x -> x + 3) 5
-- retorna 8

-- > (\x -> \y -> x * y + 5) 3 4
-- retornar 17

-- > map (\x -> x ^3) [2,4,6]
-- retorna [8,64,216]

--deriv :: Fractional a => (a -> a) -> a -> a -> a
--deriv f dx = \x -> (f(x + dx) - f(x)) / dx
 -- agora eu entendi

-- > (\(x,y) -> x * y^2) (3,4)
--retorna 48

--(\(x,y,_) -> x * y^2) (3,4,2)
--retorna 48 tmbém

-- map (\(x,y,z) -> x + y + z) [(3,4,2),(1,1,2),(0,0,4)]
-- [9,4,4]

-- > filter (\(x,y) -> x `mod` y == 0) [(4,2),(3,5),(6,3)]
--retorna todo mundo que tem o resto da divisão de x por y igual á zero
-- nesse caso, retorna [(4,2), (6,3)]

-- >  (\xs -> zip xs [1,2,3]) [4,5,6]
-- junta cada termo de xs com cada termo de [4,5,6]


-- >map (\xs -> zip xs [1..]) [[4,6],[5,7]]
-- junta cada termo de xs com cada termo de [1..] formando tuplas
-- nesse caso [[(4,1), (6,2)], [(5,1), (7,2)]]

-- >foldr1 (+) [1,2,3]
--soma toods os valores à partir da direita

--foldr1 (\x -> \y -> x + y + 7) [1,2,3,4,5]
-- também não entendi direito esse, soma os dois primeiros da
-- direita apra a esquerda mas não bate o resultado que eu penso


--questão 2
data NomeP = Nome String deriving (Show, Eq, Ord)
data SobreNomeP = SobreNome String deriving (Show, Eq, Ord)
type NomeCompleto = (NomeP,SobreNomeP) 
compara :: NomeCompleto-> NomeCompleto -> Bool
compara (Nome n1, SobreNome s1) (Nome n2, SobreNome s2) = if n1 == n2 && s1 == s2 then True else False
--compara (Nome "Ana", SobreNome "Lima") (Nome “Caio”, SobreNome "Silva") retorna falso
-- para funcionarem as outras definições é só colcoar o deriving (Eq,Ord)




-- questão 3

data Exp a = Val a -- um numero 
 | Neg (Exp a)
 | Add (Exp a) (Exp a) -- soma de duas expressoes
 | Sub (Exp a) (Exp a) -- subtracao
 | Mul (Exp a) (Exp a) -- multiplicacao
 | Div (Exp a) (Exp a) -- divisao
avalia :: Fractional a => Exp a -> a
avalia (Val x) = x
avalia (Neg exp) = - (avalia exp)
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = (avalia exp1) / (avalia exp2)


-- é só passar Val junto no parâmetro, assim:
-- >*main avalia (Add (Val 1) (Val 2))

--questão 9:
somaQuadPos :: ( Num a) => (Ord a) => [a] -> a
somaQuadPos xs = foldl (\acc x -> if x >0 then acc + x*x else acc) 0 xs