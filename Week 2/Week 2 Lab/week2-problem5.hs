-- Function which doubles all the element of a list l:: [Int] and keeps only those which are greater than 10
-- [1,3,5,7]
-- [14]

double :: Int -> Int
double x = x * 2

doubleList :: [Int] -> [Int]
doubleList xs = map double xs 

dropList :: [Int] -> [Int]
dropList xs = (dropWhile (<=10) (doubleList xs))