--Define a recursive function
--that merges two sorted lists of values to give a single sorted list.

merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys