

module Main (main) where
 main :: IO()
 prod :: Float->Float->Float->Float
 prod a b c = a*b*c
 main = do putStr"digite tres numeros"
           x <-readLn
           y <- readLn
           z <- readLn
           putStr (show (prod x y z))
           