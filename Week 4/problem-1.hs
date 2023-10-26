{-

Express the comprehension [f x | x <- xs, p x] using the functions map and filter. The function type is given as:
fun :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]

For example:
fun (^2) even [1..20]
[4,16,36,64,100,144,196,256,324,400]

fun (^2) odd [1..20]
[1,9,25,49,81,121,169,225,289,361]

-}

fun' :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]
fun' f p l = map f (filter p l)