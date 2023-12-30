{-
Recall the data type we developed in lecture for JSON data.

data Json = JNull
	  | JStr String
	  | JNum Float
	  | JBool Bool
	  | JArr [Json]
	  | JObj [(String, Json)]


Write a function

allValues :: Json -> String -> [Json]


so that allValues json key recursively finds all values associated with the string key occuring in instances of the JObj constructor.
-}

allValues :: Json -> String -> [Json]
allValues (JArr vs) key = concat $ map (\v -> allValues v key) vs  
allValues (JObj kvs) key = (concat $ map (\(_,v) -> allValues v key) kvs) ++
                       [ v | (k,v) <- kvs , k == key]
allValues _ key = []          
