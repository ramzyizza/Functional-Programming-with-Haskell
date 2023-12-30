{-
Using the datatype from the previous examples, and supposing the type a
has an instance of Show, implement a function which renders the tree
as a collection of parentheses enclosing the elements at the leaves.

showBin :: Show a => BinL a -> String


For example:

*Main> showBin (Nd (Lf 1) (Nd (Lf 3) (Lf 4)))
"((1)((3)(4)))"
-}

showBin :: Show a => BinL a -> String
showBin (Lf a) = "(" ++ show a ++ ")"
showBin (Nd l r) = "(" ++ showBin l ++ showBin r ++ ")"
