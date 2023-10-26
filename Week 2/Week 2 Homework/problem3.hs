--Define three variants of function that returns the third element in any list that contains at least this many elements

--a. using head & tail

third1 :: [a] -> a
third1 xs = head (tail (tail xs))


--b. list indexing

third2 :: [a] -> a
third2 xs = xs !! 2

--c. pattern matching
third3 :: [a] -> a
third3 (_:_:xs:_) = xs

main :: IO()
main = do
    let a = [1,2,3,4,5]
    putStrLn $ "List A = " ++ show a
    putStrLn $ "function 1 = " ++ show (third1 a)
    putStrLn $ "function 2 = " ++ show (third2 a)
    putStrLn $ "function 3 = " ++ show (third3 a)
