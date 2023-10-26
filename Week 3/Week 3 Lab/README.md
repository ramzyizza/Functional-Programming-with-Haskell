
## Recursive Functions

1. Without looking at the standard prelude, define the following library functions using recursion:

	* Decide if all logical values in a list are true:

		```hs
		and :: [Bool] -> Bool
		```
	* Concatenate a list of lists:

		```hs
		concat :: [[a]] -> [a]
		```
	* Produce a list with n identical elements:

		```hs
		replicate :: Int -> a -> [a]
		```

	* Select the nth element of a list:

		```hs
		(!!) :: [a] -> Int -> a
		```

	* Decide if a value is an element of a list:

		```hs
		elem :: Eq a => a -> [a] -> Bool
		```

1. Define a recursive function

	```hs
	merge :: Ord a => [a] -> [a] -> [a]
	```
	that merges two sorted lists of values to give a single sorted list.

	For example:

	```hs
	> merge [2,5,6] [1,3,4]
	[1,2,3,4,5,6]
	```

## List Comprehensions

Please read the following handout before proceeding further [List Comprehensions](/files/LectureNotes/Sections/list_comprehensions.md)

1. A triple (x,y,z) of positive integers is called pythagorean if
x^2 + y^2 = z^2 . Using a list comprehension, define a function:

    ```hs
    pyths :: Int -> [(Int,Int,Int)]
    ```

    that maps an integer n to all such triples with components in
    [1..n]. For example:

    ```hs
    > pyths 5
    [(3,4,5),(4,3,5)]
    ```

1. A positive integer is perfect if it equals the sum of all of its
   factors, excluding the number itself. Using a list comprehension,
   define a function

    ```hs
    perfects :: Int -> [Int]
    ```

    that returns the list of all perfect numbers up to a given limit. For example:

    ```hs
    > perfects 500
    [6,28,496]
    ```

	Many variations of this exercise are possible:

    * A number which is less than the sum of its proper divisors is called [abundant](https://en.wikipedia.org/wiki/Abundant_number).
    * A number which is greater than the sum of its proper divisions is called [deficient](https://en.wikipedia.org/wiki/Deficient_number).
    * A number for which the sum of all its divisors (including itself) is greater than
	the sum of the divisors of any smaller number is called [highly abundant](https://en.wikipedia.org/wiki/Highly_abundant_number).

	For each of these variations, write a function which finds all the numbers with the
	stated property below a given number.

1. The scalar product of two lists of integers xs and ys of length n is give by the sum of the products of the corresponding integers:

   	![dot-prod](https://github.com/ramzyizza/Functional-Programming-with-Haskell/assets/89899122/590cbb25-be5d-480f-8eb4-1dc516a717cf)


    Using a list comprehension, define a function that returns the scalar product of two lists.

1.  **Harder** Implement [matrix multiplication](https://en.wikipedia.org/wiki/Matrix_multiplication) where matrices are represented by lists of lists of integers.  One possibility, for example, would be to take the dimensions of the matrices as arguments, so that your function would have type:

    ```hs
	  matrix_mul :: Int -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
    ```
