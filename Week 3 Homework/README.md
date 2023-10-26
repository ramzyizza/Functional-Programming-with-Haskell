

# Homework for Week 3
0. Copy the file [Homework3-Template.hs](Homework3/Homework3-Template.hs) to a new file `Homework3.hs` in the same folder.
1. We classify cars into categories according to their gas usage, measured in liters per 100 kilometers.
Consider
```haskell
data Classification = Low | Medium | High | SuperHigh deriving (Show)
```
(The `deriving (Show)` is there so that Haskell can print classifications when you're testing your program in the terminal. We will discuss this in more detail later.)
Write a function
```haskell
gasUsage :: (Fractional a, Ord a) => a -> Classification
```
according to the table
| Gas Usage g        | g < 3 | 3 <= g < 5 |  5 <= g < 7 | 7 <= g    |
|--------------------|-------|------------|-------------|-----------|
| Classification     | Low   | Medium     | High        | SuperHigh |
2. (From "Programming in Haskell", Section 4.8 "Exercises", Exercise 8)
The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds as follows:
- consider each digit as a separate number;
- moving left, double every other number from the second last;
- subtract 9 from each number that is now greater than 9;
- add all the resulting numbers together;
- if the total is divisible by 10, the card number is valid.
See also the [Wikipedia entry on the Luhn algorithm](https://en.wikipedia.org/wiki/Luhn_algorithm).
Define a function
```haskell
luhnDouble :: Int -> Int
```
that doubles a digit and
subtracts 9 if the result is greater than 9. For example:
```hs
> luhnDouble 3
6
> luhnDouble 6
3
```
Using `luhnDouble` and the integer remainder function `mod`, define a function
```haskell
luhn :: Int -> Int -> Int -> Int -> Bool
```
that decides if a four digit bank card number is valid (according to the Luhn algorithm described above).
For example:
```hs
> luhn 1 7 8 4
True
> luhn 4 7 8 3
False
```
