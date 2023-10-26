{-

Harder Church Numerals
It is possible to represent the natural numbers (i.e. 0,1,2,...) using higher
order functions of type

(a -> a) -> (a -> a)


These are called Church Numerals. The encoding works like
this: the input to a function of the above type is an element f
of type a -> a.  Since such a function takes input values of
type a and also produces output values of type a, this means
it can be iterated.  So we will represent the numeral n by a
the element of type (a -> a) -> (a -> a) which iterates its
argument n times.
To see the idea, the first few examples of numerals are written like
this:

zero :: (a -> a) -> (a -> a)
zero f x = x

one :: (a -> a) -> (a -> a)
one f x = f x

two :: (a -> a) -> (a -> a)
two f x = f (f x)

three :: (a -> a) -> (a -> a)
three f x = f (f (f x))

Write a function to implement addition of Church numerals
Write a function to implement multiplication of Church numerals

-}