{-
Do the same for left grafting
-}

(\\) :: BT a -> BT a -> BT a 
(\\) Empty s = s
(\\) (Fork x l r) s = Fork x (l \\ s) r
