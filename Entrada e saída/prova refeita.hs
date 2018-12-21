import System.IO
data Produto = Prod { nome :: String, preco :: Int, cBarra :: Int, qntd :: Int} deriving (Eq,Ord,Show,Read)



escreveDados :: String -> IO()
escreveDados f =
 do h <- openFile f WriteMode
    putStrLn "escreva quantos produtos deseja inserir"
    x <- readLn
    escreveDadosDeArquivo x h
    hClose h



escreveDadosDeArquivo :: Int-> Handle-> IO()
escreveDadosDeArquivo n h
 | n == 0 = return ()
 |otherwise  = do putStr "escreva o nome do produto"
                  n1 <- getLine
                  putStr "escreva o preço do produto"
                  p <- getLine
                  putStr "escreva o código de barras do produto"
                  c <- getLine
                  putStr "escreva a quantidade do produto"
                  q <- getLine
                  hPutStrLn h (show (Prod (n1) (read p) (read c) (read q)))
                  escreveDadosDeArquivo (n-1) h
   





leDados :: String -> IO()
leDados f =
  do h <- openFile f ReadMode
     hFlush stdout
     dados <- leDadosDeArquivo h
     putStrLn ("tem " ++ show (qntdInes dados) ++ " de produtos sem estoque")
     hClose h


leDadosDeArquivo :: Handle -> IO[Produto]
leDadosDeArquivo h =
  do x <- hIsEOF h
     if x then return []
     else do y <- hGetLine h
             resto <- leDadosDeArquivo h
             return ((read y) : resto)




qntdInes :: [Produto] -> Int
qntdInes [] = 0
qntdInes ((Prod n p c x):xs) = if x == 0 then (qntdInes xs) + 1
                               else qntdInes xs 



