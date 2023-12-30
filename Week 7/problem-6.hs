{-
Define the right grafting operation
-}

(//) :: BT a -> BT a -> BT a 
(//) Empty s = s
(//) (Fork x l r) s = Fork x l (r // s)