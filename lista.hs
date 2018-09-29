
dobra :: [Int]-> [Int]
dobra [] = []
dobra(x:y) = 
 2*x:(dobra y)

dobrafol :: [Int] ->[Int]
dobrafol l = [2* y| y <- l]


concat2 :: [String] -> String
concat2 [] = []
concat2 (x:xs) =  
 x ++ (concat2 xs)




