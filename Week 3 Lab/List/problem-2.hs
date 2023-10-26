--A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.

--List comprehensioin that define a function that returns the list of all perfect numbers up to a given limit

--Example: perfects 500 = [6,28,496]

--factor function
factor :: Integer -> [Integer]
factor x = [x' | x' <- [1..x-1], x `mod` x' == 0] 

--sum factor function
totalFactor :: Integer -> Integer
totalFactor x = sum [x' | x' <- factor x]

--perfect function (list of integers)
perfects :: Integer -> [Integer]
perfects x = [x'| x' <- [1.. x], x' == totalFactor x']