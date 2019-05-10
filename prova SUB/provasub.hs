--aluno: Pedro Henrique Bufulin de Almeida
-- matrícula : 121711BCC028 

import System.IO

--1)
import Data.Char

--a)
minusc1 :: String -> String
minusc1 [] = []
minusc1 (x:xs) = toLower x : minusc1 xs

--b)
minusc2 :: String -> String
minusc2 [] = []
minusc2 (x:xs)
 | isAlpha x = minusc1 ([x] ++ minusc2 xs)
 | otherwise = minusc1 (minusc2 xs)


--c)
minusc3 :: String -> String
minusc3 x = filter  (isAlpha) (map (toLower) x)

--d)
quant :: String -> (String, Int)
quant x = (x, length (filter (isAlpha) x) )

--2)
data Produto = Prod { nome :: String , preco :: Int , codigo :: Int , qnt :: Int} deriving (Eq,Show,Ord)

escreveDados :: IO ()
escreveDados  = 
 do
 putStrLn "Escreva quantos produtos você quer armazenar no arquivo: "
 x <- getLine
 putStrLn "Escrevendo em arquivo ..."
 h <- openFile "produtos.dat" WriteMode
 escreveDadosEmArquivo (read x)  h
 hClose h

escreveDadosEmArquivo :: Int -> Handle -> IO ()
escreveDadosEmArquivo n h
 | n == 0 = return ()
 | otherwise =
 do
 putStrLn "Digite o nome do produto"
 l <- getLine
 putStrLn "Digite o preço do produto"
 p <- getLine
 putStrLn "digite o código de barras do produto"
 c <- getLine
 putStrLn "digite a quantidde de itens desse produto"
 q <- getLine
 hPutStrLn h ( show (Prod   l (read p) (read c) (read q)))
 escreveDadosEmArquivo (n-1) h

leDados :: String -> IO ()
leDados f =
 do
 h <- openFile f ReadMode
 x <- leDadosDeArquivo h
 putStrLn "produtos que não estão no estoque"
 putStrLn (show ( inexistente x))
 putStrLn "quantidade de produtos que não estão em estoque"
 putStrLn (show (quantos x))
 hClose h

leDadosDeArquivo :: Handle -> IO [Produto]
leDadosDeArquivo h =
 do
 x <- hIsEOF h
 if x
 then return []
 else do
      y <- hGetLine h
      putStrLn y
      leDadosDeArquivo h

-- função que calcula quais produtos que não existem em produtos.dat
inexistente :: [Produto] -> [Produto]
inexistente [] = []
inexistente ( _ _ _  qnt):xs
 | qnt == 0 = ( _ _ _ qnt): (inexistente xs)
 | otherwise = inexistente xs

-- função que calcula quantos produtos não existem
quantos :: [Produto] -> Int
quantos x = length (inexistente x)

