

import System.IO

escreveDados :: String -> IO ()
escreveDados f = 
     do
        putStrLn "Escrevendo em arquivo ..."
        h <- openFile f WriteMode
        escreveDadosEmArquivo 5 h
        hClose h

escreveDadosEmArquivo :: Int -> Handle -> IO ()
escreveDadosEmArquivo n h
  | n == 0 = return ()
  | otherwise =
       do
          putStr "Digite um valor:"
          l <- getLine
          hPutStrLn h l
          escreveDadosEmArquivo (n-1) h

leDados :: String -> IO ()
leDados f = 
     do
        h <- openFile f ReadMode
        y <- leDadosList h
        --putStrLn (show (foldr (\acc x -> acc + x) 0 y)) -- soma a lista de dados
        --putStrLn (show y)  -- escreve os dados em lista
      
        hClose h

leDadosDeArquivo :: Handle -> IO ()
leDadosDeArquivo h =
   do
     x <- hIsEOF h
     if x
     then return ()
     else
       do
        y <- hGetLine h
        putStrLn y
        leDadosDeArquivo h

leDadosList :: Handle -> IO ([Int])
leDadosList h = 
 do
  x<-hIsEOF h
  if x 
  then return ([])
  else do y <- hGetLine h
          resto <-leDadosList h
          return ((read y): resto)



data Shape = Circle Float Float Float | Rectangle Float Float 