-- Rewrite the head and tail functions from the prelude so that they use the Maybe type constructor to indicate when provided the argument was empty.

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe (_:xs) = Just xs
