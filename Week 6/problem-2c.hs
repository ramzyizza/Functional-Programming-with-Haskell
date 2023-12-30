-- A common use of the Either type constructor is to return information about a possible error condition.  Rewrite the function zip from the prelude so that we only get the list of pairs when the two arguments have
the same length.  If this is not the case, use the String to report which argument was smaller.

zipEither :: [a] -> [b] -> Either String [(a,b)]
zipEither [] [] = Right []
zipEither [] (_:_) = Left "Left list too small"
zipEither (_:_) [] = Left "Right list too small"
zipEither (x:xs) (y:ys) =
  case zipEither xs ys of
	Left msg -> Left msg
	Right r -> Right ((x,y):r)
