-- The scalar product of two lists of integers xs and ys of length n is give by the sum of the products of the corresponding integers:

--Using a list comprehension, define a function that returns the scalar product of two lists.

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [(x * y) | (x,y) <- zip xs ys]