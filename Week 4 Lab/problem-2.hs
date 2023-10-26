{- 

Redefine map f and filter p using foldr and foldl. For your reference, here are the definitions of map and filter from lecture notes. HINT. Read about the foldr and foldl functions in the handout higher-order functions and Chapter 7.3 and 7.4 of Programming in Haskell.

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
   | p x       = x : filter p xs
   | otherwise = filter p xs
   
-}

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\y ys -> (f y):ys) [] xs
{-   [1,2,3]
     (f(1):(f(2):(f(3):[])))
     
     map' (+1) [] [1,2,3] => acc = []
     foldr(3 [] -> (1 + 3):[]) => acc = [4]
     foldr(2 4 -> (1 + 2):[4]) => acc = [3,4]
     foldr(1 [3,4] -> (1 + 1):[3,4]) => acc = [2,3,4]
     
     result: [2,3,4]
-}

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldl(\ys y -> ys ++ [f y]) [] xs
{-
    [1,2,3]
    ((([] ++ 1) ++ 2) ++ 3) 
    
    map'' (+1) [] [1,2,3] => acc = []
    foldl([] 1 -> [] ++ (1+1)) => acc = [2]
    foldl([2] 2 -> [2] ++ (1+2)) => acc = [2,3]
    foldl([2,3] 3 -> [2,3] ++ (1+3) => acc = [2,3,4]
    
    result: [2,3,4]
-}

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\y ys -> if f y then y:ys else ys) [] xs

-- [1,2,3], acc = []
-- (1:(2:(3:[])))

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f xs = foldl(\ys y -> if f y then ys ++ [y] else ys) [] xs

-- [1,2,3], acc = []
-- ((([]++1)++2)++3)