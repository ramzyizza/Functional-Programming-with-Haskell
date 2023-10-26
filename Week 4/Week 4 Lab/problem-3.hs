{-

Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternatively applies the two argument functions to successive elements in a list.
For example:

> altMap (+10) (+100) [0,1,2,3,4]
[10,101,12,103,14]

-}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = foldr(\y ys -> if length ys `mod` 2 == 0 then (f y):ys else (g y):ys) [] xs