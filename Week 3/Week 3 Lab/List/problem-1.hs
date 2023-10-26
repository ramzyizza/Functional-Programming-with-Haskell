--List comprehension that maps an integer n to all such triples with component in [1..n]

--A triple (x,y,z) of positive integeres is called pythagorean if x^2 + y^2 = z^2

--Given function pyths :: Int -> [(Int, Int, Int)]

--Example: pyths 5 = [(3,4,5) ,(4,3,5)]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
