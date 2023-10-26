## Writing More Functions

1.  Write a function to pair each element of a list with its index.
    
2.  Using guarded equations, write a function of type `Int -> Int -> Bool` that returns `True` if the first argument is greater than the second and less than twice the second.
    
3.  (Adapted and expanded from the book "Programming in Haskell) Define three variants of a function `third :: [a] -> a` that returns the third element in any list that contains at least this many elements, using
    
    1.  `head` and `tail`
        
    2.  list indexing `!!`
        
    3.  pattern matching
        
4.  (Adapted and expanded from the book "Programming in Haskell) Define a function `safetail :: [a] -> [a]` that behaves like tail except that it maps `[]` to `[]` (instead of throwing an error). Using `tail` and `isEmpty :: [a] -> Bool`, define `safetail` using
    
    1.  a conditional expression
        
    2.  guarded equations
        
    3.  pattern matching
