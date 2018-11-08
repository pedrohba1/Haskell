data Dia = Dom | Seg | Ter | Quar | Quin | Sex | Sab 



finadeSemana Sex = True
finadeSemana Sab = True
finadeSemana _ = False




data Est = Verao | Outono | Inverno | Primavera


instance Eq Est
 where (==) Verao Verao = True
       (==) Inverno Inverno = True 
       (==) Primavera Primavera = True
       (==) Outono Outono = True
       (==) _ _ = False


--outra forma de fazer o mesmo que o código acima é 
-- data Est = Verao | Outono | Inverno | Primavera
-- deriving Eq


data Pessoa = Ind Name Sobrenome AnoNascimento	
 deriving (Show)
type Name = String
type Sobrenome = String
type AnoNascimento = Int
 


data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr

eval :: Expr->Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)



data Lista a  = Nil | Cons a (Lista a)


quant :: (Lista a) -> Int
quant Nil = 0
quant (Cons _ b) = quant b +1 	






