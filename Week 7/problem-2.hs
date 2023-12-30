{-
Using the datatype from the previous problem, write a function

leaves :: BinLN a b -> [a]

which collects the list of elements decorating the leaves of the
given tree.
-}

leaves :: BinLN a b -> [a]
leaves (Leaf x) = [x]
leaves (Node l _ r) = leaves l ++ leaves r 
