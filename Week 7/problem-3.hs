{-
Implement a new version of binary trees which carries data only
at the leaves.

data BinL a = ???
-}

data BinL a = Lf a | Nd (BinL a) (BinL a)