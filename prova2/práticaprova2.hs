until' :: (a->Bool) -> (a->a) -> a-> a
until' f g h
 |f h = h
 |otherwise =  until' f g ( g h)






