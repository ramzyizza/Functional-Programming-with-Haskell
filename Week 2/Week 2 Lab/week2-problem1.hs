--Function that returns true if at least one argument is True

--Using Guards
orB :: Bool -> Bool -> Bool
orB x y | x || y == True = True
        | otherwise      = False

--Using If Else
orB2 :: Bool -> Bool -> Bool
orB2 x y = if x || y == True then True else False