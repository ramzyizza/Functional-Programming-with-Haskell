# Test 1

## Question 1 — Even majority (**8 marks**)
**Task** Write the following function `evenMajority`, that takes a list of
integers, and tells whether more than half of them are even.
```haskell
evenMajority :: [Int] -> Bool
evenMajority ns = undefined
```
## Question 2 — 5-smooth numbers (**8 marks**)
A 5-smooth number is an integer which has no prime factor larger than 5. For an
integer N, we define S(N) as the set of 5-smooth numbers less than or equal to
N. For example S(20) = {1,2,3,4,5,6,8,9,10,12,15,16,18,20}
**Task** Define the following function `get5SmoothNumbers`, that gives a list of
all 5-smooth numbers less than or equal to a given number.
```haskell
get5SmoothNumbers :: Int -> [Int]
get5SmoothNumbers k = undefined
```
Examples:
```hs
*Test1> get5SmoothNumbers 25
[1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25]
*Test1> get5SmoothNumbers 50
[1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36,40,45,48,50]
```
## Question 3 — Train stops (**8 marks**)
Consider the following type of train stops on the West Midlands Railway line,
from Redditch to Birmingham New Street.
```haskell
data TrainStop = BirminghamNewStreet
 | FiveWays
 | University
 | SellyOak
 | Bournville
 | KingsNorton
 | Northfield
 | Longbridge
 | BarntGreen
 | Alvechurch
 | Redditch
 deriving (Eq, Show)
```
We define the function `theStopAfter` on this type, which encodes the
information of which stop comes immediately after which stop.
```haskell
theStopAfter :: TrainStop -> TrainStop
theStopAfter Redditch            = Alvechurch
theStopAfter Alvechurch          = BarntGreen
theStopAfter BarntGreen          = Longbridge
theStopAfter Longbridge          = Northfield
theStopAfter Northfield          = KingsNorton
theStopAfter KingsNorton         = Bournville
theStopAfter Bournville          = SellyOak
theStopAfter SellyOak            = University
theStopAfter University          = FiveWays
theStopAfter FiveWays            = BirminghamNewStreet
theStopAfter BirminghamNewStreet = undefined
```
Note that the function is undefined on `BirminghamNewStreet` because that is the
last possible stop on this line. You should ensure that this function is never
called on `BirminghamNewStreet`, because the program will crash if you do that.
**Task** Define the following function `comesBefore`, using the given
`theStopAfter` function, such that `comesBefore s1 s2` is `True` if and only if
`s1` is a stop preceding stop `s2`.
```haskell
comesBefore :: TrainStop -> TrainStop -> Bool
comesBefore s1 s2 = undefined
```
Some examples:
```hs
*Test1> comesBefore University BirminghamNewStreet
True
*Test1> comesBefore Bournville FiveWays
True
*Test1> comesBefore BirminghamNewStreet University
False
*Test1> comesBefore University KingsNorton
False
*Test1> comesBefore University University
False
```
## Question 4 — Repeated applications of a function (**8 marks**)
**Task** Write a function `countApplications`
```haskell
countApplications :: (a -> a) -> (a -> Bool) -> a -> Int
countApplications f p x = undefined
```
that takes
 1. a function `f :: a -> a`,
 1. a termination condition `p :: a -> Bool`, and
 1. an input `x :: a`,
and counts the number of times that the function `f` must be repeatedly applied
to `x` until the output satisfies the condition `p`.
Here is an example of how to use this function:
```hs
*Test1> countApplications (\n -> n `div` 2) odd 8
3
*Test1> countApplications (\n -> n `div` 2) odd 10
1
```
We will only test your implementation of `countApplications` on functions that
do terminate with respect to the termination condition.
## Question 5 — Higher order functions (**8 marks**)
**Task** Write a function `f` of the following type
```haskell
f :: (a -> a -> r) -> ((a -> r) -> a) -> r
f g h = undefined
```
The function should terminate for all terminating inputs. Your solution should
not use recursion or `undefined`.