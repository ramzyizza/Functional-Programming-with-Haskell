--Define function safetail that behaves like tail except that it maps [] to [] instead of trowing error. Using tail and isEmpty :: [a] -> Bool

--a. Using conditional Expression
isEmpty :: [a] -> Bool
isEmpty xs = if length xs == 0 then True else False
safeTail :: [a] -> [a]
safeTail xs = if isEmpty xs then [] else tail xs

--b. Using guarded equations
isEmpty2 :: [a] -> Bool
isEmpty2 xs | length xs == 0 = True
            | otherwise      = False
            
safeTail2 :: [a] -> [a]
safeTail2 xs | isEmpty2 xs = []
             | otherwise   = tail xs

--c. pattern matching

{- isEmpty3 :: [a] -> Bool
isEmpty3 [] = True
isEmpty3 [_:_] = False
-}

safeTail3 :: [a] -> [a]
safeTail3 [] = []
safeTail3 (x:xs) = xs

main :: IO()
main = do
    let a = [1,2,3,4]
    let b = []
    putStrLn $ "Conditional Expression on list a: " ++ show (safeTail a :: [Int])
    putStrLn $ "Conditional Expression on list b: " ++ show (safeTail b :: [Int])
    putStrLn $ "Guarded Equation on list a: " ++ show (safeTail2 a :: [Int])
    putStrLn $ "Guarded Equation on list b: " ++ show (safeTail2 b :: [Int])
    putStrLn $ "Pattern Matching on list a: " ++ show (safeTail3 a :: [Int])
    putStrLn $ "Pattern Matching on list b: " ++ show (safeTail3 b :: [Int])