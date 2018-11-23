module Main (main) where
 main :: IO()
 main = do putStr"Digite um número:"
           s1 <- getLine
           putStr"Digite outro número:"
           s2 <- getLine
           putStr"Soma dos números digitados:"
           putStrLn (show (read  s1+ read  s2))