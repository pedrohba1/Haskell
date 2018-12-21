import System.IO

zipWith' :: (Eq a) => [a]->[a] ->[(a,a)]
zipWith' _ [] = []
zipWith' [] _ = []
zipWith' [] [] = []
zipWith' (a:as) (b:bs) = [(a,b)] ++ zipWith' as bs


  
zipWith'' :: (Eq a) =>((a,a)->(a,a))-> [a]->[a] ->[(a,a)]
zipWith'' f  _ [] = []
zipWith'' f [] _ = []
zipWith'' f [] [] = []
zipWith'' f s s2 = map f (zip s s2)

data Motorista = Mot { anoDeNas :: Int , sexo :: Char, chave :: Int } deriving (Eq,Show,Ord,Read)


escreveDados :: String -> IO()
escreveDados f = do putStrLn "escrevendo em arquivo ..."
                    h <- openFile f WriteMode
                    escreveDadosEmArq 2 h
                    hClose h


escreveDadosEmArq :: Int -> Handle -> IO()
escreveDadosEmArq n h
 |n == 0 = return ()
 |otherwise = do putStrLn "digite o ano de nascimento"
                 a <- readLn
                 putStrLn "digite 'M'para masculino ou 'F' para feminino"
                 s <- readLn
                 putStrLn "digite a chave de registro"
                 c <- readLn
                 hPutStrLn h (show ( Mot a s c))
                 escreveDadosEmArq (n-1) h



leDados:: String ->IO()
leDados f = do h <-openFile f ReadMode
               motor<- leDadosArquivo h
               putStrLn ("o percentual de mulheres em acidentes eh: " ++ show (((totalMulheres motor) * 100)/(totalAcidentes motor)) ++ "%")
               hClose h


leDadosArquivo:: Handle -> IO([Motorista])
leDadosArquivo h = do x <- hIsEOF h 
                      if x then return []
                      else do y <- hGetLine h
                              resto <- leDadosArquivo h
                              return ((read y):resto)




totalAcidentes :: [Motorista] -> Float
totalAcidentes [] = 0
totalAcidentes (x:xs) = 1 + totalAcidentes xs 


totalMulheres :: [Motorista] -> Float
totalMulheres [] = 0
totalMulheres ((Mot _ s _):xs)
 | s == 'F' =  (totalMulheres xs) +1 
 | otherwise = totalMulheres xs