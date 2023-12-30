# Test 2

## Question 1 (**12 marks**)

Consider the function `penta` defined below.

```haskell
penta :: Integer -> Integer
penta 0 = 0
penta 1 = 1
penta 2 = 2
penta 3 = 3
penta 4 = 4
penta n =     penta (n-5)
        + 2 * penta (n-4)
        - 3 * penta (n-3)
        + 4 * penta (n-2)
        - 5 * penta (n-1)
```

This function runs in exponential time. We want to define a more
efficient version `pentaFast` using the state monad. __Do not__ change
the definition of `pentaFast`.  If you do, you will receive zero marks
on this question.

```
pentaFast :: Integer -> Integer
pentaFast n = a
  where
    ((),(a,b,c,d,e)) = runState (statePenta n) (0,1,2,3,4)

statePenta :: Integer -> State (Integer,Integer,Integer,Integer,Integer) ()
statePenta = undefined
```

**Task** Complete the definition of `statePenta` so that `penta n == pentaFast
n` for every `n :: Integer` and `pentaFast` runs in linear time. Do not modify
the function `pentaFast`.

## Question 2 (**12 marks**)

You have been asked to write a banking application that keeps track of current
account balance and account activity (e.g. deposits, withdrawals, etc). The
following data type definitions have been proposed:

```haskell
type CurrBalance = Int

data Transaction = Deposit
                 | Withdrawal
                 | DepositFailed
                 | WithdrawalFailed
                 deriving (Eq, Show)

type BankAccount = (CurrBalance, [Transaction])
```

**Task** Implement the following function `deposit` to make a deposit into the
bank account.

```haskell
deposit :: Int -> State BankAccount ()
deposit amount = undefined
```

* The `deposit` function uses the state monad to keep track of the account balance
as well as the history of account activities. 
* The state used for this is a pair `(CurrBalance, [Transaction])`,
where the first component, `CurrBalance`, is the current balance
and the second component, `[Transaction]`, is a list of transactions,
**in chronological order**.
* In order to count as a valid deposit, the deposited amount must be
greater than `0`. In case of invalid input amount, the function does
not change the `CurrBalance` but **logs a deposit failure with `DepositFailed`**.

For example, the following function
```haskell
transactions1 :: State BankAccount ()
transactions1 = do deposit 1000
                   deposit (-100)
                   deposit 500
                   deposit 300
```
gives the output below when executed with initial balance of `0`:

```hs
> runState (transactions1) (0, [])
((),(1800,[Deposit,DepositFailed,Deposit,Deposit]))
```

**Task** Implement the following function `withdraw` to make a withdrawal from
the bank account.

```haskell
withdraw :: Int -> State BankAccount ()
withdraw amount = undefined
```

* The `withdraw` function also uses the state monad to keep track of
  `CurrBalance` and a log of account activity in `[Transaction]` **in
  chronological order**.
* In order to count as a valid withdrawal, the withdrawal amount must
  be **greater than `0` and less than or equal to the current account
  balance**.
* In case of invalid input the function does not change the `CurrBalance` but
  **logs a withdrawal failure using `WithdrawalFailed`**.

For example, the following function
```haskell
transactions2 :: State BankAccount ()
transactions2 = do deposit 500
                   withdraw 700
                   deposit 100
                   withdraw 1200
```
gives the output below when executed with initial balance of `1000`:

```hs
> runState (transactions2) (1000, [])
((),(900,[Deposit,Withdrawal,Deposit,WithdrawalFailed]))
```

## Question 3 (**12 marks**)

Consider the type of binary trees storing data at the nodes:

```haskell
data Bin a = Leaf | Node a (Bin a) (Bin a)
```

Write a function
```haskell
runAll :: Monad m => Bin (m a) -> m (Bin a)
runAll = undefined
```
which converts a tree labelled with monadic values to a single monadic tree using a pre-order traversal. 

For example, the following trees in the `Maybe` and `List` monads
```haskell
ex1 :: Bin (Maybe Int)
ex1 = Node (Just 4) (Node (Just 7) Leaf Leaf) (Node (Just 1) Leaf Leaf)

ex2 :: Bin (Maybe Bool)
ex2 = Node (Just True) Leaf (Node Nothing Leaf Leaf)

ex3 :: Bin [Int]
ex3 = Node [4] (Node [7] Leaf Leaf) (Node [1,2] Leaf Leaf)

ex4 :: Bin [Int]
ex4 = Node [17,45] Leaf (Node [4,3] Leaf Leaf)
```
give:
```
> runAll ex1
Just (Node 4 (Node 7 Leaf Leaf) (Node 1 Leaf Leaf))
> runAll ex2
Nothing
> runAll ex3
[Node 4 (Node 7 Leaf Leaf) (Node 1 Leaf Leaf),Node 4 (Node 7 Leaf Leaf) (Node 2 Leaf Leaf)]
> runAll ex4
[Node 17 Leaf (Node 4 Leaf Leaf),Node 17 Leaf (Node 3 Leaf Leaf),Node 45 Leaf (Node 4 Leaf Leaf),Node 45 Leaf (Node 3 Leaf Leaf)]
```
Notice that the last list has **four** trees and that the order of the result depends on the order of the traversal.

## Question 4 (**12 marks**)

We can represent logical expressions with the following datatype:

```haskell
data Expr = Var     Char
          | Not     Expr
          | And     Expr Expr
          | Or      Expr Expr
          | Implies Expr Expr
          deriving (Eq, Show)
```

Some examples:
  - The singleton formula `p` would be represented as `Var 'p'`.
  - The formula `p && q` would be represented as `And (Var 'p') (Var 'q')`.
  - The expression `p && (q || (r ==> q))` would be expressed as
    `And (Var 'p') (Or (Var 'q') (Implies (Var 'r') (Var 'q')))`.

The implementation of logical expressions using digital circuits is
usually done using _only one_ type of logical operation, the so-called
Nand operation, due to ease of manufacturing and power consumption
(among other reasons).  In other words, circuit manufacturers use the
following alternate representation of logical expressions, which we will call *circuits*.

```haskell
data Circuit = Input Char
             | Nand  Circuit Circuit
             deriving (Eq, Show)
```

We use the symbol `_⊼_` to denote the `Nand` operation, which is the Boolean
function given by the following table:

| `p` | `q` | `p ⊼ q`    |
|-----|-----|------------|
| 0   | 0   |    1       |
| 0   | 1   |    1       |
| 1   | 0   |    1       |
| 1   | 1   |    0       |

Your task in this question is to write a function transforming an expression
into a circuit given the following elementary facts about Boolean logic.

  1. The expression `¬ p` is equivalent to `p ⊼ p`.
  1. The expression `p ∧ q` is equivalent to `¬ (p ⊼ q)`.
  1. The expression `p ∨ q` is equivalent to `¬ ((¬ p) ∧ (¬ q))`.
  1. The expression `p ⇒ q` is equivalent to `(¬ p) ∨ q`.

No other logical facts are needed to complete this question.

**Task** Implement the following function `circuit` that transforms a logical
expression into an equivalent circuit.

```haskell
circuit :: Expr -> Circuit
circuit exp = undefined
```

For example:
```hs
> circuit (Not (Var 'p'))
Nand (Input 'p') (Input 'p')
> circuit (And (Var 'p') (Var 'q'))
Nand (Nand (Input 'p') (Input 'q')) (Nand (Input 'p') (Input 'q'))
> circuit (Or (Not (Var 'p')) (Var 'q'))
Nand (Nand (Nand (Nand (Nand (Input 'p') (Input 'p')) (Nand (Input 'p') (Input 'p'))) (Nand (Input 'q') (Input 'q'))) (Nand (Nand (Nand (Input 'p') (Input 'p')) (Nand (Input 'p') (Input 'p'))) (Nand (Input 'q') (Input 'q')))) (Nand (Nand (Nand (Nand (Input 'p') (Input 'p')) (Nand (Input 'p') (Input 'p'))) (Nand (Input 'q') (Input 'q'))) (Nand (Nand (Nand (Input 'p') (Input 'p')) (Nand (Input 'p') (Input 'p'))) (Nand (Input 'q') (Input 'q'))))
```

## Question 5 (**12 marks**)

We define a _MinHeap_ to be a binary tree with the property that every
path from the root to a leaf has increasing values.  In particular,
the root of every subtree is the smallest value of that subtree.  We
will use the following data type to describe MinHeap:

```haskell
data Heap a = Empty
            | HeapNode a (Heap a) (Heap a)
            deriving (Eq, Show)
```

Write a function
```haskell
insert :: Ord a => a -> Heap a -> Heap a
insert x heap = undefined
```
which takes a value and inserts it into the heap tree while
respecting the min heap property.

Examples:
```hs
      50                           20
     /  \                         /  \
   100   200   insert 20 ->      50   200
   / \   / \                    / \   / \
  E   E E   E                 100  E E   E
                              / \
                             E   E


      50                          50
     /  \                        /  \
   100   200   insert 150 ->   100   150
   / \   / \                   / \    / \
  E   E E   E                 E   E  200 E
                                     / \
                                    E   E
```
*Note*: The letter `E` indicates an `Empty` node in the examples above.

In Haskell, this takes the form:
```hs
> insert 20 (HeapNode 50 (HeapNode 100 Empty Empty) (HeapNode 200 Empty Empty))
HeapNode 20 (HeapNode 50 (HeapNode 100 Empty Empty) Empty) (HeapNode 200 Empty Empty)

> insert 150 (HeapNode 50 (HeapNode 100 Empty Empty) (HeapNode 200 Empty Empty))
HeapNode 50 (HeapNode 100 Empty Empty) (HeapNode 150 (HeapNode 200 Empty Empty) Empty)
									
```
Popping the minimum value from a heap involves removing the root node
and moving the smaller value of the two subtrees to the root node.
For example:

```hs
          4                                       8
        /   \                                   /   \
       8     10              popMin ->         9     10
      / \    / \                              / \    / \
     9   E  E   20                           E   E  E   20
    / \         / \                                     / \
   E   E       E   E                                   E   E

```
*Note*: The letter `E` indicates an `Empty` node in the examples above.

**Task** Implement the `popMin` function that returns the minimum
value from the heap (or nothing if the heap is empty) as well as the
remaining heap.

```haskell
popMin :: Ord a => Heap a -> (Maybe a, Heap a)
popMin heap = undefined
```

For example:
```hs
> popMin (HeapNode 4 (HeapNode 8 Empty Empty) (HeapNode 10 Empty (HeapNode 20 Empty Empty)))
(Just 4,HeapNode 8 Empty (HeapNode 10 Empty (HeapNode 20 Empty Empty)))

> popMin (HeapNode 4 (HeapNode 8 (HeapNode 9 Empty Empty) Empty) (HeapNode 10 Empty (HeapNode 20 Empty Empty)))
(Just 4,HeapNode 8 (HeapNode 9 Empty Empty) (HeapNode 10 Empty (HeapNode 20 Empty Empty)))
```
