## Introduction
Consider the structure of a list
```Haskell
data [a] where
    (:) :: a -> [a] -> [a]
    []  :: [a]
```
`foldr cons nil` is a function that collapses a list by replacing the empty list with `nil` and `(:)` with `cons`:
```Haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr cons nil [] = nil
foldr cons nil (x : xs) = cons x (foldr cons nil xs)
```
The purpose of `foldr` is to collapse a list of type `[a]` into a value of type `b`. In this sense, we see that `cons` has the same type as `(:)`, except that `[a]` is replaced with `b`, and `nil` has the same type as `[]`, except that `[a]` is also replaced with `b`.

This kind of fold can be generalised to a function `f` that acts on any data structure `t` with `N` constructors and `M` type parameters, where
- The first `N` arguments of `f` correspond to the `N` value constructors of `t` (for instance `cons` and `nil` with `(:)` and `[]` respectively)
- The `N + 1`th argument corresponds to `t`
- `f` returns a value of some polymorphic type `b`
- `f` replaces each `i`th constructor by the `i`th argument, and then recurses on each nested `t`, should a nested `t` exist

## Examples
For Booleans
```Haskell
data Bool where
    False :: Bool
    True :: Bool
```
There are two value constructors to replace, so two expressions of type `a` must be passed in. The third argument is a `Bool`, and the return type is `a`. This describe the `bool` function, which is defined in `Data.Bool`:
```Haskell
bool :: a -> a -> Bool -> a
bool true false True = true
bool true false False = false
```

`bool` is a shorthand for an `if` expression; it is particularly useful if we need to partial the condition.

For `Maybe`
```Haskell
data Maybe a where
    Nothing :: Maybe a
    Just    :: a -> Maybe a
```
There are two value constructors to replace. `Nothing` must be replaced by an expression of type `b`, and `Just` by a function of type `a -> b`. We also need to pass in a `Maybe a` and return a `b`. This describes the `maybe` function given in `Data.Maybe`:

```Haskell
maybe :: b -> (a -> b) -> Maybe a -> b
maybe nothing just Nothing  = nothing
maybe nothing just (Just x) = just x
```

`maybe` gives us an alternative to extracting a value from a `Maybe` rather than having to use `fromJust` and case expressions (since `fromJust` crashes if called on `Nothing`). In fact, `fromJust` is equivalent to `maybe undefined id`

For `Nat`
```Haskell
data Nat where
    Z :: Nat
    S :: Nat -> Nat
```
There are two value constructors to replace. `Z` must be replaced by an expression of type `a` and `S` must be replaced by a function of type `a -> a`, since all `Nat`s must be replaced with `a`s. Hence

```Haskell
nat :: a -> (a -> a) -> Nat -> a
nat z s Z = z
nat z s (S n) = s (nat z s n)
```

`Nat` is a recursive data type so for `S n`, `n` must be folded to an `a` before we can call `s`.

The `nat` function is a very elegant way of representing mathematical induction; `z` represents the base case and `s` the inductive step, as it describes how to go from a solution in terms of '`n - 1`' to '`n`'. With this in mind, we can use `nat` to define alternative implementations for `toInt`, `addNat`, `multNat` and `powNat`

```Haskell
toInt :: Nat -> Int
toInt = nat 0 (+ 1)

-- Base case is when m = 0; then result is n + 0 = n
-- Inductive step is when we have calculated n + (m - 1); we
-- then need to add 1 so we apply `S`
addNat :: Nat -> Nat -> Nat
addNat n m = nat n S m

-- Base case is when m = 0; then result is n * 0 = 0
-- Inductive step is when we have calculated n * (m - 1); we
-- then need to add n to get n * m
multNat :: Nat -> Nat -> Nat
multNat n m = nat Z (addNat n) m

-- Base case is when m = 0; then result is n^0 = 1
-- Inductive step is when we have calculated n^(m - 1); we
-- then need to multiply by n to get n^m
powNat :: Nat -> Nat -> Nat
powNat n m = nat (S Z) (multNat n) m
```

For `Bush`
```Haskell
data Bush a where
    Leaf :: a -> Bush a
    Fork :: Bush a -> Bush a -> Bush a
```

There are two value constructors. We need to replace `Leaf` with an `a -> b` function, and `Fork` with a `b -> b -> b` function. A `Bush a` then needs to be passed and a `b` returned. We therefore defined `bush` as follows

```Haskell
bush :: (a -> b) -> (b -> b -> b) -> Bush a -> b
bush leaf fork (Leaf x)     = leaf x
bush leaf fork (Fork lb rb) = fork (bush leaf fork lb) (bush leaf fork rb)
```
`sumBush` finds the total of all values stored at every leaf in the `bush`:

```Haskell
sumBush :: Num a => Bush a -> a
sumBush = bush id (+)
```

The implementation for `mapBush` (`fmap` for `Bush`), can also be rewritten more elegantly as follows
```Haskell
mapBush :: (a -> b) -> Bush a -> Bush b
mapBush f = bush (Leaf . f) Fork
```
The structure of the bush needs to be maintained so we have to reconstruct each `Leaf` after applying `f` as well as each `Fork`.

`bush` can also be elegantly used in the monadic `bind` for Bush, which was implemented as follows
```Haskell
(>>=) :: Bush a -> (a -> Bush b) -> Bush b
(>>=) (Leaf t) f = f t
(>>=) (Fork lt rt) f = Fork (lt >>= f) (rt >>= f)
```
We see that `(>>=)` applies a function to transform each `Leaf` into another `Bush`. `bindBush` can be rewritten as follows using `bush`
```Haskell
bindBush :: Bush a -> (a -> Bush b) -> Bush b
bindBush t f = bush f Fork t
```
In the base case, `f` is applied to each `Leaf`, which produces a new `Bush` at each leaf. In the inductive step, `Fork` is applied to each transformed `Bush` in order to maintain the general structure of the original `Bush`.

For a `Tree`
```Haskell
data Tree a where
    Tip :: Tree a
    Node :: Tree a -> a -> Tree a -> Tree a
```
We have two value constructors. For `Tip`, we pass in a `b`. For `Node`, we pass in `b -> a -> b -> b`. Lastly, we pass in a `Tree a` and return `b`. We define `tree` as follows

```Haskell
tree :: b -> (b -> a -> b -> b) -> Tree a -> b
tree tip node Tip            = tip
tree tip node (Node lt x rt) = node (tree tip node lt) x (tree tip node rt)
```

As with `Bush`, we can define `mapTree` more elegantly as follows
```Haskell
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = tree Tip (\lt x rt -> Node lt (f x) rt)
```
Unlike `Bush`, we cannot simply pass `Node` as the second function since we need to apply `f` to `x` in order to transform it into a `b`

For `Rose`
```Haskell
data Rose a where
    Flower :: a -> Rose a
    Vine :: [Rose a] -> Rose a
```

There are two value constructors. `Flower` needs to be replaced with an `a -> b` function. `Vine` needs to be replaced by a `[b] -> b` function; we pass in `[b]` since each of the `Rose`s in the vine must have been transformed to `b. We can now defined `rose` as follows

```Haskell
rose :: (a -> b) -> ([b] -> b) -> Rose a -> b
rose flower vine (Flower x) = flower x
rose flower vine (Vine ts) = vine (map (rose flower vine) ts)
```

Now consider `Either`:
```Haskell
data Either a b where
    Left :: a -> Either a b
    Right :: b -> Either a b
```
We have two value constructors; `Left` must be replaced with a `a -> c` function (`Either a b` cannot be transformed to `b` as before since `b` already refers to another polymorphic type) and `Right` must be replaced with a `b -> c` function. We then define `either` as
```Haskell
either :: (a -> c) -> (b -> c) -> Either a b -> c
either left right (Left x)  = left x
either left right (Right y) = right y
```