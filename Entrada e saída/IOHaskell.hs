import Data.Char
import System.IO
import Control.Monad

ecoLine :: IO ()
ecoLine = do input <- getLine
             intput <-getLine
             putStr input
             putStr intput




ask :: String -> IO String
ask question = do putStr question
                  getLine



main :: IO ()
main = do nome <- ask "Qual ´e o seu nome? "
          matr <- ask "Qual ´e o seu n´umero de matr´ıcula?"
          putStrLn ("Benvindo "++ nome ++ "!")
          putStrLn ("Seu n´umero de matr´ıcula ´e "++ matr)




             


--1)Escreva um programa que lˆe uma linha, a partir do teclado, verifica se ela cont´em apenas
--caracteres alfab´eticos e imprime essa linha na tela, com as palavras em ordem inversa.
--Caso a linha contenha algum caractere n˜ao alfab´etico, imprime uma mensagem de errocheca :: String -> Bool


checa ::String -> Bool
checa [] = True
checa (x:xs)
 | x == '\n' = True
 | fromEnum x >= 65 && fromEnum x <= 90 = checa xs
 | fromEnum x>= 97 && fromEnum x <= 122 = checa xs
 | otherwise = False


lelinha :: IO ()
lelinha = do li <- ask "escreve uma palavra ae"
             if checa li == True then putStrLn (reverse li)
             else error "tem coisa que nao eh letra ae"


--2. Escreva um programa que pergunta ao usu´ario o seu nome e telefone e imprime na tela a
--	informa¸c˜ao obtida, em uma ´unica linha.

nometel :: IO ()
nometel = do n <- ask "escreva seu nome "
             t <- ask "escreva seu telefone "
             putStrLn ("nome: " ++ n ++ " Telefone: " ++ t)



--3. Escreva um programa que lˆe v´arias linhas a partir do teclado, e imprime cada linha lida,
--com os caracteres convertidos para mai´usculas, at´e que seja digitada uma linha nula.

maisc :: IO ()
maisc = do line <- getLine
           if null line
              then return ()
           else do  putStrLn (map toUpper line)
                    maisc
                  
                   

--ou também dessa maneira, que escreve tudo primeiro e printa
--maiúsculo depois:
f4 :: IO()
f4 = do
      input <- getLine
      if(input == "") then return ()
      else do f4
              putStrLn (map toUpper input)


--1.4)
leInt :: IO(Int)
leInt = do putStr "Digite um valor inteiro: "
           readLn


leInt2 :: IO(Int)
leInt2 = do putStr "Digite um valor inteiro: "
            n <- getLine
            return (read n)

soma2 :: IO ()
soma2 = do n1 <- leInt
           n2 <- leInt
           putStr "A soma ´e: "
           print (n1+n2)

-- Defina uma fun¸c˜ao leIntList que lˆe para uma seq¨uˆencia de valores inteiros do dispositivo
--de entrada padr˜ao, at´e que seja digitado o valor 0, e retorna a lista dos valores lidos.




-- Defina um programa que lˆe uma valor inteiro positivo n e imprime a lista de pares (i, i2
--para valores de i no intervalo 1 ≤ i ≤ n.
uneemtupla :: [Int]->[Int]->[(Int,Int)]
uneemtupla [] [] = []
uneemtupla (a:as) (b:bs) = [(a,b)] ++ (uneemtupla as bs)

imprimeIntervalo :: IO ()
imprimeIntervalo = do i <- leInt
                      print (uneemtupla [1 .. i] [i *i | i <- [1..i]])

