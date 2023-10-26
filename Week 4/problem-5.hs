-- Define it with comprehensions and no recursion. HINT: You will need two generators, one to extract a list xs from the list of lists xss, and another to extract an element x from the list xs, and put this in the result of the comprehension.
concat1 :: [[a]] -> [a]
concat1 xss = [x | xs <- xss, x <- xs] --first approach (work)

-- --Define the same function using recursion instead. HINT. Find and use the prelude function ++.
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = x ++ concat2 xs

-- --Define the same function using foldr and foldl, and without recursion or list comprehensions.
concat3 :: [[a]] -> [a]
concat3 xss = foldr(\y ys -> y++ys) [] xss
-- [[1,2,3],[4,5],[6,7]] = []
-- ([1,2,3]:([4,5]:([6,7]:[])))


concat4 :: [[a]] -> [a]
concat4 xss = foldl(\ys y -> ys ++ y) [] xss
-- [[1,2,3],[4,5],[6,7]] = []
-- ((([]++[1,2,3])++[4,5])++[6,7])

--Testing for equality
list = [[2,3,4],[5,6,7],[8,9,10]]
concat1_test = concat1 list == concat list
concat2_test = concat2 list == concat list
concat3_test = concat3 list == concat list
concat4_test = concat4 list == concat list
testAll = and [concat1_test, concat2_test, concat3_test, concat4_test]


--Testing for Speed
biglist = replicate 1000 [1..1000]
concat1_test2 = concat1 biglist == concat biglist
concat2_test2 = concat2 biglist == concat biglist
concat3_test2 = concat3 biglist == concat biglist
concat4_test2 = concat4 biglist == concat biglist

--Testing for N size

nlist n = replicate n [1..n]
concat1_test3 n = concat1 (nlist n) == concat (nlist n)
concat2_test3 n = concat2 (nlist n) == concat (nlist n)
concat3_test3 n = concat3 (nlist n) == concat (nlist n)
concat4_test3 n = concat4 (nlist n) == concat (nlist n)
