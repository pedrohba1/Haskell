module Main (main) where
 main :: IO()
 main =  do putStrLn "Digite uma frase"
            frase <- getLine 
            if frase == reverse frase then putStrLn "eh palindro" else  putStrLn "nao eh"