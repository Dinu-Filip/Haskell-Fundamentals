## Laziness vs. strictness
A key feature of Haskell is that is uses **call-by-need evaluation**, also known as **lazy evaluation**, where the evaluation of an expression is delayed until its values is needed. If there are multiple occurrences of the same expression, Haskell will cache the result of the expression when it is first evaluated and then reuse it at each occurrence. In contrast, in **strict evaluation**, arguments are always evaluated fully before being substituted.

Importantly, lazy functions will always return an answer if it is possible to produce one, whereas strict functions may not always produce an answer even if it were possible. When a lazy function always evaluates everything passed to it and produces the same result as a strict function, the strict function will be more efficient.

Laziness has a wide range of uses in Haskell. For example, we can model infinity as follows
```Haskell
infinity :: Integer
infinity = infinity + 1
```

`infinity` will cause the program to hang when its evaluation is forced, but it can be passed lazily to functions as needed. For instance, if we have the following function
```Haskell
two :: a -> Int
two _ = 2
```

Calling `two infinity` will return 2 since `infinity` is not used anywhere in the function.

An example of a function which is not lazy, however, includes the following
```Haskell
fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n - 1)
```

Because of pattern matching, `n - 1` must be evaluated to check if it is equal to 1.

## Enforcing strictness
In certain situations, enforcing strictness where lazy evaluation would otherwise take place can improve performance. Suppose, for example, that we wish to sum the numbers in a list. Using `foldr`, we can do this tail recursively, but the list ends up being summed as follows
```Haskell
  foldl (+) 0 [1, 2, 3]
= foldl (+) (0 + 1) [2, 3]
= foldl (+) ((0 + 1) + 2) [3]
= foldl (+) (((0 + 1) + 2) + 3) []
= (((0 + 1) + 2) + 3)
= ((1 + 2) + 3)
= (3 + 3)
= 6
```
There is no reason for the accumulating parameter to be evaluated lazily, so the alternative `foldl'` from `Data.List` can be used to enforce the strict evaluation of the accumulator:
```Haskell
  foldl' (+) 0 [1, 2, 3]
= foldl' (+) (0 + 1) [2, 3]
= foldl' (+) 1 [2, 3]
= foldl' (+) (1 + 2) [3]
= foldl' (+) 3 [3]
= foldl' (+) (3 + 3) []
= foldl' (+) 6 []
= 6
```
From Haskell 2021, we can enforce strict evaluation using the `BangPatterns` extension and the `(!)` operator, for example we can defined `foldl'` as follows
```Haskell
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f !acc []      = acc -- ! only needed on first occurrence of strict argument
foldl' f acc (x : xs) = foldl' f (f acc x) xs
```

A good rule of thumb to know when to force strict evaluation on a function `f` with argument `x` is as follows
- `f` calls a function on a function of `x` e.g. `h (g x)`
- `f` is not already strict in `x`
