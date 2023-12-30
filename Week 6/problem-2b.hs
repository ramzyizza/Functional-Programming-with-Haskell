-- Similarly, rewrite take :: Int -> [a] -> [a] to use Maybe to indicate when the index is longer than the list.

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe n [] | n == 0 = Just []
                          | otherwise = Nothing
takeMaybe 0 x = Just []
takeMaybe n (x:xs) =
    case takeMaybe (n-1) xs of
          Nothing -> Nothing
          Just r -> Just (x:r)