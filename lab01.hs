

--3:
double no = no + no

--4:
quad no = double (double no)

--5:

soma2 x y = x + y

--6:
misterio a b c d = soma2 (soma2 a b) (soma2 c d)

--7:
soma4 a b c d = a + b + c + d
--outros trem	
hip a b = sqrt(a*a + b*b)

zeroto :: Int -> [Int]
zeroto n = [0..n]

isDigit :: Char->Bool
isDigit c = c>='0' && c <= '9'

olhaimc imc |imc <= 18.5 = "ganhe peso"
            |imc <= 25.0 = "ótimo"
            |imc <= 30.0 = "gordo"
            |otherwise = "muito acima do peso"


--guardas
abs n |n>= 0=  n
      |n<0 =  -n
