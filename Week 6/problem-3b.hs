-- Show that the type Maybe a is a retract of the type [a].

toList :: Maybe a -> [a]
toList Nothing = []
toList (Just x) = [x]

toMaybe :: [a] -> Maybe a
toMaybe [] = Nothing
toMaybe (x:_) = Just x