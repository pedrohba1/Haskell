import System.IO

data Produto = Prod { nome :: String , preco :: Float , codBarras :: Int ,qntEstoque :: Int} deriving (Show,Read,Eq,Ord)

escreveDados :: String -> IO ()
escreveDados f = 
     do
        putStrLn "Escrevendo em arquivo ..."
        h <- openFile f WriteMode
        escreveDadosEmArquivo h
        hClose h

escreveDadosEmArquivo ::  Handle -> IO ()
escreveDadosEmArquivo h = do
          putStr "Digite o nome do Produto:"
          l <- getLine
          putStr "Digite um valor para o produto:"
          p <- getLine
          if (read p) == -1 then return () -- quando digitado -1 a função para de ler os dados
          else do putStr "Digite o codigo de barras do produto:"
                  q <- getLine
                  putStr "Digite a quantidade em estoque do produto:"
                  s <- getLine
          
                  hPutStrLn h (show (Prod (l) (read p) (read q) (read s)))
                  escreveDadosEmArquivo h


leDados :: String -> IO ()
leDados f = 
     do
        h <- openFile f ReadMode
        r <- leDadosDeArquivo h
        putStrLn (show (listaProdutos r))
        putStrLn "quantidade de elementos com falta no estoque : "
        putStrLn (show (confereEstoque r))
        putStrLn (show (listaFaltas r))
        hClose h

leDadosDeArquivoSTRING :: Handle -> IO [String]
leDadosDeArquivoSTRING h =
   do
     x <- hIsEOF h
     if x
     then return []
     else
       do
        y <- hGetLine h
        resto <- leDadosDeArquivoSTRING h
        return ((y): resto)
       
leDadosDeArquivo :: Handle -> IO [Produto]
leDadosDeArquivo h =
   do
     x <- hIsEOF h
     if x
     then return []
     else
       do
        y <- hGetLine h
        resto <- leDadosDeArquivo h
        return ((read y): resto)

listaProdutos :: [Produto]->[String]
listaProdutos [] = []
listaProdutos ((Prod n _ _ _):xs) = (n:listaProdutos xs)

confereEstoque :: [Produto] -> Int
confereEstoque [] = 0
confereEstoque ((Prod _ _ _ qnt):xs) = if qnt == 0 then (confereEstoque xs)+1 else confereEstoque xs

listaFaltas :: [Produto] -> [String]
listaFaltas [] = []
listaFaltas ((Prod n _ _ q):xs) = if q == 0 then ((n):listaFaltas xs) else listaFaltas xs
