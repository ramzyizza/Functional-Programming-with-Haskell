--Using guarded equation, return true if the first argument is greater than the second argument and less than twice the second.

func :: Int -> Int -> Bool
func x y | x > y && x < 2 * y = True
         | otherwise          = False
