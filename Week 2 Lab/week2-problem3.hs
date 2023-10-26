--Function that remove both the first and the last element of a list
import Control.Monad

removeFirstLast :: [a] -> [a]
removeFirstLast xs = tail (reverse (tail (reverse xs)))

a = [1,2,3,4]
result = removeFirstLast a

main :: IO ()
main = do
    putStrLn $ "Result of taking first and last element from list a = [1,2,3,4] = " ++ show result
    
{-4321
321
123
23 
-}