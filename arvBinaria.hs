data ArvBin a = Vazia | No (ArvBin a) a (ArvBin a)
instance (Eq a) => Eq (ArvBin a) where
 Vazia == Vazia = True
 (No e1 x1 d1) == (No e2 x2 d2) = x1== x2 && e1 == e2 && d1 == d2
 _ == _ = False





data Cor = Azul | Vermelho 
instance Show Cor where
 show (Azul) = "oi"
 show (Vermelho) =  "aeaeaeea"