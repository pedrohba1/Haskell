import System.IO

data Produto = Prod {nome::String, preco::Float, codigoDeBarras::Int, qtd::Int} deriving (Show, Eq, Ord, Read)

leArq :: String -> IO ()
leArq f = do putStrLn "Escrevendo no arquivo:"
             h <- openFile f WriteMode
             putStrLn "Quantos produtos deseja cadastrar?"
             x <- readLn
             escreva x h
             hClose h

escreva :: Int->Handle->IO()
escreva n h  
 | n == 0 = return ()
 | otherwise =
   do putStrLn "Digite o nome do produto"
      l <- getLine
      putStrLn "Digite o preco do produto"
      p <- getLine
      putStrLn "Digite o codigo do produto"
      c <- getLine
      putStrLn "Digite o quantidade do produto"
      q <- getLine
      hPutStrLn h (show ((Prod (l) (read p) (read c) (read q))))
      escreva (n-1) h

     