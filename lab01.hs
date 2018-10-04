
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

media3 a b c = (a + b + c) /2

maior1 :: Int->Int->Int
maior1 a b = if a >b then a
else b

maior2 a b 
 | a>b = a
 | otherwise = b


maior2' :: Int->Int->Int
maior2' a b = if a >b then a
             else if b>a then b
                  else a

maior3 :: Int->Int->Int->Int
maior3 a b c = if a>b && b>c then a
               else if a<b && b>c then b
                    else c

maior3' :: Int->Int->Int->Int
maior3' a b c
 | a>b && b>c = a
 | a<b && b>c = b
 | otherwise = c


maior3'' :: Int->Int->Int->Int
maior3'' a b c = maior1 (maior1 a b) (maior1 b c)

numRaizes :: Float->Float->Float->Int
numRaizes a b c 
 |sqrt(b*b - 4*a*c) > 0.0 = 2
 |sqrt(b*b - 4*a*c) == 0.0 = 1
 |otherwise = 0
 

misterio2 m n p = not( m==n&&n==p)

--guardas
abs n |n>= 0=  n
      |n<0 =  -n
