-- Function which returns the reverse of a list if its length is greater than 7. 

reverseList :: [a] -> [a]
reverseList xs  | length xs > 7 = reverse xs
                | otherwise     = xs

-- Modify the function so that the cutoff length is a parameter (input)

reverseListInput :: Int -> [a] -> [a]
reverseListInput x xs   | length xs > x = reverse xs
                        | otherwise     = xs
