type Nome = String
type Preco = Int
type CodBar = Int
type ListaDeCodigos = [CodBar]
type Recibo = [(Nome,Preco)]
type BaseDeDados = [(CodBar,Nome,Preco)]


listaDeProdutos :: BaseDeDados

listaDeProdutos = [(1234, "Oleo DoBom, 1l" , 195),
                   (4756, "Chocolate Cazzeiro, 250g" , 180),
                   (3216, "Arroz DoBom, 5Kg", 213),
                   (5823, "Balas Pedregulho, 1Kg ", 379),
                   (4719, "Queijo Mineirim, 1Kg ", 449),
                   (6832, "Iogurte Maravilha, 1Kg ", 499),
                   (1112, "Rapadura QuebraDente, 1Kg", 80),
                   (1111, "Sal Donorte, 1Kg", 221),
                   (1113, "Cafe DoBom, 1Kg", 285),
                   (1115, "Biscoito Bibi, 1Kg", 80),
                   (3814, "Sorvete QGelo, 1l", 695)] 

formataCentavos :: Int -> String
formataCentavos p
                   |qntcent < 10 = show (div p 100) ++".0"++ show (rem p 100)
                   |otherwise = show (div p 100) ++"."++ show (rem p 100)
                   where qntcent =  p `mod` 100


formataLinha :: (Nome,Preco) -> String
formataLinha (a,b) = a ++ (replicate npontos '.') ++" U$"++ formataCentavos b
 where
 npontos = tamLinha - length a - length proc
 proc = formataCentavos b

repete :: Char -> Int -> [Char]
repete p 1 = [p]
repete p n = p:(repete p (n-1))

formataLinhas :: [(Nome,Preco)] -> String
formataLinhas [] = []
formataLinhas (x:xs) = formataLinha x ++ "\n" ++ (formataLinhas xs)

geraTotal :: Recibo -> Preco
geraTotal [] = 0
geraTotal ((a,b):xs) = b + geraTotal xs

formataTotal :: Preco -> String
formataTotal a = "Total" ++ repete '.' 16 ++ "$" ++ formataCentavos a

formataRecibo :: Recibo -> String
formataRecibo p = "Supermercado QLegal" ++ formataLinhas p ++ formataTotal (geraTotal p )

acha :: BaseDeDados -> CodBar -> (Nome,Preco)
acha [] num = ("item desconhecido",0)
acha (x:xs) num = if num == pegaCod x then pegaProd x else acha xs num 

pegaProd ::(CodBar,Nome,Preco) -> (Nome,Preco)
pegaProd (a,b,c) = (b,c) 
pegaCod::(CodBar,Nome,Preco) -> CodBar
pegaCod (a,b,c) = a

fazRecibo :: ListaDeCodigos -> Recibo
fazRecibo [] = []
fazRecibo (x:xs) = (acha listaDeProdutos x) : fazRecibo xs

gerarRecibo :: ListaDeCodigos -> String
gerarRecibo lc = formataRecibo( fazRecibo lc)

tamLinha :: Int
tamLinha = 30

--formataLinha :: (Nome,Preco) -> String 

