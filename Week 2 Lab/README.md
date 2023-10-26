## Functions in Haskell

Here are some example functions which you can try to implement to get started using both the concepts from the lecture as well as the library functions you found above.

1.  Write a function `orB :: Bool -> Bool -> Bool` that returns `True` if at least one argument is `True`.
    
2.  Write a function `swap :: (a, b) -> (b, a)` that swaps the elements of a pair.
    
3.  Write a function that removes both the first and the last element of a list.
    
4.  Write a function which returns the reverse of a list if its length is greater than 7. Now modify the function so that the cutoff length is a parameter.
    
5.  Write a function which doubles all the elements of a list `l :: [Int]` and then keeps only those which are greater than 10.
    
6.  Write a function to return the reverse of a string with all its alphabetic elements capitalized. (The function `toUpper :: Char -> Char` in the `Data.Char` library may be useful here.)
