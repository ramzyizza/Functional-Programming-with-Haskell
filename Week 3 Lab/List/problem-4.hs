{- a = number of row of matA
   b = number of column of matA and row of matB
   c = number of column of matB
   d = matA [list of list]
   e = matB [list of list]
-}

import Data.List (transpose)


matrix_mul :: Int -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
matrix_mul  a b c d e   | a /= length d || b /= length (head d) || c /= length (head e)  = error "Not Valid Dimension"
                        | otherwise = multMat d e
                        
multMat :: [[Int]] -> [[Int]] -> [[Int]]
multMat d e = [[sum (zipWith (*) d' e') | e' <- transpose e] | d' <- d]

{- 
let d = [[1, 2, 3], [4, 5, 6]]  -- 2x3 matrix
let e = [[7, 8], [9, 10], [11, 12]]  -- 3x2 matrix


[1,2,3]
[4,5,6]

x

[7,8]
[9,10]
[11,12]

(Transpose)
[7,9,11]
[8,10,12]

[[58, 64], [139, 154]] 

[(1*7),(2*9),(3*11)]

-}