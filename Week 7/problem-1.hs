{-
Define a type of binary trees

data BinLN a b = ???

which carries an element of type a at each leaf, and an element
of type b at each node.
-}

data BinLN a b = Leaf a | Node (BinLN a b) b (BinLN a b)