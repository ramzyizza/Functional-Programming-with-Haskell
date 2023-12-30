{-
Given a binary tree, let us label the leaves from left to right starting at 0.  Each node then determines a pair of integers (i,j) where i is the index of its left-most leaf and j is the index of its rightmost leaf.  Write a function:

leafIndices :: BT a -> BT (Int,Int)
leafIndices = undefined


Which replaces each node with the pair (i,j) of indices of its left and right-most leaves.
-}

leafIndicesAcc :: Int -> BT a -> (BT (Int,Int) , Int)
leafIndicesAcc i Empty = (Empty,i+1)
leafIndicesAcc i (Fork _ l r) =
  let (l',i') = leafIndicesAcc i l
	  (r',i'') = leafIndicesAcc i' r
  in (Fork (i,i''-1) l' r' , i'')

leafIndices :: BT a -> BT (Int, Int)
leafIndices t = fst $ leafIndicesAcc 0 t