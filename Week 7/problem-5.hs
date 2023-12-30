{-
Harder Can you write a function which, given such a well parenthesized string of numbers, produces the corresponding tree?  You may want to use Maybe or Either to report when this string is ill-formed.  (You may wish to look up the read function for help converting strings to integer types.)
-}

type MaybeReader a = String -> Maybe (a,String)

mrInt :: MaybeReader Int
mrInt s = case reads s of
  [(i,r)] -> Just (i,r)
  _ -> Nothing

mrLeftParen :: MaybeReader ()
mrLeftParen ('(':r) = Just ((),r)
mrLeftParen _ = Nothing

mrRightParen :: MaybeReader ()
mrRightParen (')':r) = Just ((),r)
mrRightParen _ = Nothing

mrSeq :: MaybeReader a -> MaybeReader b -> MaybeReader (a,b)
mrSeq x y s =
  case x s of
	Nothing -> Nothing
	Just (a,r) ->
	  case y r of
		Nothing -> Nothing
		Just (b,q) -> Just ((a,b),q)

mrChoice :: MaybeReader a -> MaybeReader b -> MaybeReader (Either a b)
mrChoice x y s =
  case x s of
	Nothing -> case y s of
				 Nothing -> Nothing
				 Just (b,r) -> Just (Right b, r)
	Just (a,r) -> Just (Left a , r)

mrParens :: MaybeReader a -> MaybeReader a
mrParens x s =
  case (mrLeftParen `mrSeq` (x `mrSeq` mrRightParen)) s of
	Nothing -> Nothing
	Just (((),(a,())),r) -> Just (a,r)

parseLeaf :: MaybeReader (BinL Int)
parseLeaf s = case mrParens mrInt s of
  Nothing -> Nothing
  Just (i,r) -> Just (Empty i,r)

parseBranch :: MaybeReader (BinL Int)
parseBranch s =
  let br = mrParens (parseBin `mrSeq` parseBin) in
	case br s of
	  Nothing -> Nothing
	  Just ((l,r),rem) -> Just (Branch l r , rem)

parseBin :: MaybeReader (BinL Int)
parseBin s =
  case mrChoice parseLeaf parseBranch s of
	Nothing -> Nothing
	Just (Left b,r) -> Just (b,r)
	Just (Right b,r) -> Just (b,r)

