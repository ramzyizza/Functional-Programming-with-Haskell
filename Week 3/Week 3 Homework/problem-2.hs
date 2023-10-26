--[a b c d]           
--[1 7 8 4]
--[c b a]
--[8 7 1]
--[16 7 2]
--[7 7 2]
--16 mod 10 = 6 | 10 - 6 = 4 (Check Digit)

luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = x * 2 - 9
             | otherwise = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | isLuhn    = True
             | otherwise = False
             where 
             isLuhn      = d == checkDigit
             checkDigit  = 10 - ((luhnDouble c + luhnDouble a + b) `mod` 10)
