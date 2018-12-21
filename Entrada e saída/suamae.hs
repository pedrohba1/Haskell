import Data.Char
import System.IO
import Control.Monad

lelinha :: IO ()
lelinha = do l <- getLine
             if l == "" then return () 
             else do lelinha
                     putStrLn (map toUpper l)



printaTuplaQuad :: Int -> [(Int, Int)]
printaTuplaQuad 0 = []
printaTuplaQuad n = [(n, n^2)] ++ printaTuplaQuad (n-1)

listQuad :: IO ()
listQuad = do n <- readLn
              print (reverse (printaTuplaQuad n)) 

