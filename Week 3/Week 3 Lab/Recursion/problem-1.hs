--Decide if all logical values in a list are true:
--and :: [Bool] -> Bool

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and'(xs)

--Concatenate a list of lists:
-- concat :: [[a]] -> [a]

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- [[1,2],[3,4,5]]
-- [1,2] ++ concat [[3,4,5]]
-- [1,2] ++ [3,4,5] ++ concat []
-- [1,2] ++ [3,4,5] ++ []
-- [1,2,3,4,5]


--Produce a list with n identical elements:
-- replicate :: Int -> a -> [a]
replicate' :: Int -> a -> [a]
replicate' n x  | n <= 0    = []
                | otherwise = [x] ++ replicate' (n-1) x
                
                
--Select the nth element of a list:
 -- (!!) :: [a] -> Int -> a

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) n  | n == 0             = x
                | otherwise          = xs !!! (n-1)           

--Decide if a value is an element of a list:
-- elem :: Eq a => a -> [a] -> Bool
elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) | n == x     = True
                |otherwise = elem' n (xs)               