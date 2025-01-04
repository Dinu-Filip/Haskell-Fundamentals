# Functors
## Definition
The `Functor` typeclass describes types whose contents can be mapped over while preserving their structure. Its general form is as follows
```Haskell
class Functor (t :: * -> *) where
    fmap :: (a -> b) -> t a -> t b
```

From the class declaration, it is clear that for a type to be a functor, it must have a kind of `* -> *`. For example, `Maybe` has kind `* -> *` since it takes some concrete type `a` and returns `Maybe a`. `Either`, however, has kind `* -> * -> *` so `Either` alone cannot be a functor, but `Either a` has kind `* -> *` which can be a functor. Therefore, *types that have more than one type parameter can only be functors in the last type parameter*. 

Examples of functors include the following
```Haskell
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)
```

```Haskell
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Tip = Tip
    fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt)
```

```Haskell
instance Functor (x ->) where
    fmap :: (a -> b) -> (x -> a) -> (x -> b)
    fmap = (.)
```

It is clear from the type signature of `fmap` for `x ->` that mapping a function over another function is the same as function composition.

Note that `->` has a kind of `* -> * -> *` which doesn't match the kind `* -> *` to be used directly in a functor, so it must be partially applied.

```Haskell
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
```

However, `-> x` cannot be a functor, since there cannot exist some implementation of `fmap` such that `fmap :: (a -> b) -> (a -> x) -> (b -> x)`.
## Lifting
Since `->` is right-associative, the type of `fmap` is equivalently
```Haskell
fmap :: (a -> b) -> (t a -> t b)
```

So partially applying a function to `fmap` returns a function `f a -> f b`. This known as **lifting** a function, which refers to applying a function over some values that are in a context. This will be essential when discussing applicatives and monads.

For example, the expression `fmap (*2)` is a function that takes a functor $t$  over numbers and returns a functor over numbers; the function `*2` is *lifted* to work in the functor $t$. $t$ may be a `Maybe`, `Either String` or a list.

## Functor laws
Functors must obey some laws

<center> <strong>First functor law:</strong> <code>fmap id = id</code> </center>

<center> <strong>Second functor law:</strong> for any functor <code>F</code>, <code>fmap (f . g) F = fmap f (fmap g F)</code> </center>

These laws are not enforced in Haskell, but it is a very strong convention to ensure that any functors obey these laws, as it gives us some guarantees about their behaviour.

**Proof of functor laws for `Maybe`**
1st functor law for `Maybe`
```Haskell
fmap id Nothing = Nothing = id Nothing
fmap id (Just x) = Just (id x) = Just x = id (Just x)
```

2nd functor law for `Maybe`
```Haskell
(fmap f . fmap g) Nothing = fmap f (fmap g Nothing)
                          = fmap f (g Nothing)
                          = f (g Nothing)
                          = (f . g) Nothing

(fmap f . fmap g) (Just x) = fmap f (fmap g (Just x))
                           = fmap f (Just (g x))
                           = Just ((f . g) x)
                           = fmap (f . g) Just x
```

**Proof of functor laws for `x ->`**
1st functor laws for `x ->`
```Haskell
fmap id f = id . f = f
```

2nd functor law for `x ->`
```Haskell
(fmap f . fmap g) h = fmap f (fmap g h)
                    = (.) f ((.) g h)
                    = f . (g . h)
                    = (f . g) . h
                    = (.) (f . g) h
                    = fmap (f . g) h
```

# Applicatives
## Motivation
Consider the function definition below for the safe division of two integers
```Haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv m n = Just (div m n)
```

If we had
```Haskell
mx = safeDiv 5 2
my = safeDiv 8 3
```
`safeDiv` is a functor, but if we wanted to add them together, we wouldn't be able to use `fmap` since `fmap` only maps over a single functor while preserving its structure; it cannot combine two functors. To do this, we need to introduce applicatives.
## Definition
The definition of the `Applicative` typeclass is as follows
```Haskell
class Functor t => Applicative t where
    pure :: a -> t a

    (<*>) :: t (a -> b) -> t a -> t b
    (<*>) = liftA2 ($)

    liftA2 :: (a -> b -> c) -> t a -> t b -> t c
    liftA2 f mx my = f <$> mx <*> my
```

By definition (not just the class declaration!) all applicatives must be functor since applicatives must still be able to be mapped over. Similar to how `(==) and (/=)` are defined in the `Eq` typeclass, we only need to provide an implementation for either `liftA2` or `(<*>)` and we get the other automatically.

`pure` is a function that takes a value and returns a minimal context that still yields that value. `pure` is needed for use to be able to implement `fmap` in terms of `liftA2`
```Haskell
fmap f mx = liftA2 ($) (pure f) mx
          = pure f <*> mx
```

Hence `pure` is used to lift a function into the applicative context. For this reason, it is useful to think of `pure` as `liftA0`, and `fmap` as `liftA1`
```Haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
```

The power of `Applicative` is in the way that `<*>` allows functions to be partially applied over applicatives. This means that functions with any number of arguments can be applied by chaining `<*>`.

## Examples
```Haskell
instance Applicative Maybe where
    pure :: a -> Maybe a
    pure x = Just x

    liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
    liftA2 f (Just x) (Just y) = Just (f x y)
    liftA2 _ _ _ = Nothing
```

```Haskell
instance Applicative (Either e) where
    pure :: a -> Either e a
    pure y = Right y

    liftA2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
    liftA2 f (Left x) _ = Left x
    liftA2 f (Right x) (Left y) = Left y
    liftA2 f (Right x) (Left y) = Left y
    liftA2 f (Right x) (Right y) = Right (f x y)
```

Hence if just one of the `Either`s is a `Left`, then we always return a `Left` since we will want to know about a failed computation. If both are `Left`, then we made the choice to always return the first `Left`, although there is also a way to combine the results of the two `Left`s.

```Haskell
instance Applicative [] where
    pure :: a -> [a]
    pure x = [x]

    (<*>) :: [a -> b] -> [a] -> [b]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

In the instance implementation above, every function in a list of functions is applied with every element in the other list. This is because lists can be viewed as **non-deterministic computations**; there is no defined way in which to pairwise apply a function to the corresponding element of a list, so they are simply combined in every way possible (i.e. like with the Cartesian product). The other possibility is like with `zipWith`, where a function is applied to the elements *at the same index* in both arrays. For this, a separate [[5 Types and typeclasses#^531657|newtype]], `ZipList`, is defined, which is discussed later on.

```Haskell
instance Applicative ((->) x) where
    pure :: a -> (x -> a)
    pure = const

    (<*>) :: (x -> a -> b) -> (x -> a) -> (x -> b)
    (<*>) f g x = f x (g x)

    -- liftA2 not needed if bind implemented, but makes use case
    -- for applicative (x ->) clearer
    liftA2 :: (a -> b -> c) -> (x -> a) -> (x -> b) -> (x -> c)
    liftA2 f g h x = f (g x) (h x) 
```

The applicative instance of `x ->` is useful for parameters that are operated on by multiple functions, for example
```
(+) <$> (*2) <*> (+3) :: Num a => a -> a
```

is equivalent to the mathematical expression
```
(x * 2) + (x + 3)
```
## Applicative laws
Like functors, applicatives should obey thing laws to ensure they behave as expected


<center> <strong>First applicative law:</strong> <code> pure id &lt;*&gt; v = v</code> </center>

<center> <strong>Second applicative law:</strong> <code> pure f &lt;*&gt; pure x = pure (f x)</code> </center>

<center> <strong>Third applicative law:</strong> <code> u &lt;*&gt; pure y = pure ($ y) &lt;*&gt; u</code> </center>

<center> <strong>Fourth applicative law:</strong> <code> pure (.) &lt;*&gt; u &lt;*&gt; v &lt;*&gt; w = u &lt;*&gt; (v &lt;*&gt; w)</code> </center>

The first law ensures that `pure id` does nothing (just like the `id` function by itself). 
The second law ensures that `pure` preserves function application; applying a pure function to a pure value is the same as applying the function directly to the value and then using `pure`.
The third law ensures that applying a **morphism** (function wrapped in an applicative) to a pure value is the same as supplying the value as an argument in a `pure` to the morphism.
The fourth law ensures that `pure (.)` composes morphisms similar to how `(.)` composes functions.

A fifth law, which states that `fmap f x = pure f <*> x`, can be derived from the four laws above, although this was implicitly covered in the section above.

**Proof of applicative laws for `Maybe`**
1st applicative law for `Maybe`
```
pure id <*> Nothing = Nothing

pure id <*> (Just x) = Just (id x)
                     = Just x
```

2nd applicative law for `Maybe`
```
pure f <*> pure x = pure f <*> (Just x)
                  = Just (f x)
                  = pure (f x)
```

3rd applicative law for `Maybe`
```
u <*> pure y = u <*> (Just y)
             = (Just f) <*> (Just y)
             = Just (f $ y)
             = Just (($ y) f)
             = (Just ($ y)) <*> (Just f)
             = pure ($ y) <*> u
```

4th applicative law for `Maybe`
```
pure (.) <*> Just x <*> Just y <*> Just z = Just (.) <*> Just x <*> Just y <*> Just z
                                          = Just (x .) <*> Just y <*> Just z
                                          = Just (x . y) <*> Just z
                                          = Just ((x . y) z)
                                          = Just (x (y z))
                                          = Just x <*> (Just (y z))
                                          = Just x <*> (Just y <*> Just z)
```

# Monads
## Motivation
Going back to the `safeDiv` implementation
```Haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv m n = Just (div m n)
```

Suppose that we wanted to lift `safeDiv` so that it would be able to work on `Maybe`. Then `liftA2 safeDiv` would have type `Maybe Int -> Maybe Int -> Maybe (Maybe Int)`. `Maybe (Maybe Int)` is not the kind of result we expect, and neither functors nor applicatives give us a way to collapse this structure down. To do this, we need monads.

## Definition
The `Monad` typeclass has the following definition
```Haskell
class Applicative m => Monad m where
    return :: a -> m a
    
    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ => y
```

`return` is equivalent to `pure`; it is a historical remnant of when monads were first invented, and bears no relation to the return control sequence in imperative languages.

The function passed to `bind` allows us to choose the next computation to run based on the results of the first computation. When chaining applicatives, the structure of each applicative needed to be known beforehand. Using `bind`, monads allow computations to be chained where the structure of the computation is not known in advance; we want the result to be based on some intermediate results.

Much like `(*>)` for the Applicative typeclass, `(>>)` allows both computations to be carried out while ignoring the result of the first computation.

As an alternative to `>>=`, Monad comes with another function called `join` that we can implement instead with signature `join :: Monad m => m (m a) -> m a`. Join is used to flatten structures, for example
```Haskell
joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe Nothing = Nothing
joinMaybe (Just mx) = mx

-- Type signature matches concat!
-- This also gives some intuition for why >>=
-- for lists is equivalent to concatMap
joinList :: [[a]] -> [a]
joinList = concat
```

Using `join`, we can expression `(>>=)` as follows
```Haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
mx >>= f = join (fmap f mx)
```

Similar to how we have `liftA2` for applicatives, we also have `liftM2` for monads
```Haskell
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f mx my = mx >>= (\x -> fmap (f x) my)
```

and we can express `<*>` in terms of the monadic `ap`
```Haskell
ap :: Monad m => m (a -> b) -> m a -> m b
ap mx my = do
    f <- mx
    a <- my
    pure (f a)
```

so we can express applicative operations in terms of monadic operations
## `do` notation
`do` is a syntactic sugar as a replacement for using many binds and lambdas in a sequence of computations. For example, instead of writing
```Haskell
Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
```

we can instead write
```Haskell
do
    x <- Just 3
    y <- Just "!"
    pure (show x ++ y)
```

`<-` can be thought as using bind with a lambda. Not including a `<-` is equivalent  Importantly, we can only bind a monadic value to a variable with `<-` within the `do` block; `x` in the example above cannot be 'accessed' outside the `do` block. This is one reason why monads are used to hide side effects in IO actions.

Since `do` is no different to bind, the result of the entire monadic computation is simply the result of the last monadic value in the `do` block.

Although `do` notation resembles imperative code, each line in a `do` block is not 'independent' of the other line; the context of each line is considered, for example
```Haskell
do
    x <- Just 3
    y <- Just "!"
    Nothing
    pure (show x ++ y)
```

returns `Nothing` instead of `Just "3!"`. This `do` block is equivalent to
```Haskell
Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing >>= (\_ -> Just (show x ++ y))))
```

In general, `do` notation is translated as follows
```
do { x }                     = x

do { x; <stmts> }            = x >> do { <stmts> }

do { v <- x ; <stmts> }      = x >>= \v -> do { <stmts> }

do { let <decls> ; <stmts> } = let <decls> in do { <stmts> }
```
## Examples
```Haskell
instance Monad Maybe where
    return :: a -> Maybe a
    return x = Just x

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= f = Nothing
    Just x >>= f  = f x
```

```Haskell
instance Monad [] where
    return :: a -> [a]
    return x = [x]

    (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = concatMap f xs
```

Much like with applicatives, monadic operations on lists are also non-deterministic. In particular, list comprehensions are simply syntactic sugar for monadic operations on lists, that is,
```Haskell
listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1, 2]
    ch <- ['a', 'b']
    return (n, ch)
```
is equivalent to
```Haskell
listOfTuples :: [(Int, Char)]
listOfTuples = [(n, ch) | n <- [1, 2], ch <- ['a', 'b']]
```

```Haskell
instance Monad ((->) x) where
    return :: a -> (x -> a)
    return = const

    (>>=) :: (x -> a) -> (a -> (x -> b)) -> (x -> b)
    (>>=) f g x = g (f x) x
```

The monad instance for `x ->` can be used to combine a unary function and binary function to create a new unary function in situations where the same parameter needs to be passed to multiple functions. For example, `(*2) >>= (+)` is equivalent to the expression `(x * 2) + x` (a weird way of multiplying by 3).

A more complex example is to make `Bush a` an instance of `Monad`, defined as
```Haskell
data Bush a = Leaf a | Fork (Bush a) (Bush a) deriving (Show, Functor)
```

```Haskell
instance Monad Bush where
    -- A minimal structure for a Bush is a Leaf
    return :: a -> Bush a
    return x = Leaf x

    -- Bush (Bush a) has `Bush`es at its `leaves`
    join :: Bush (Bush a) -> Bush a
    join (Leaf t) = t
    join (Fork lt rt) = Fork (join lt) (join rt)

    (>>=) :: Bush a -> (a -> Bush b) -> Bush b
    (>>=) (Leaf t) f = f t
    (>>=) (Fork lt rt) f = Fork (lt >>= f) (rt >>= f)
```

We can do some interesting things with monadic `Bush`
```Haskell
sprout :: Int -> Bush Int
sprout n = Fork (Leaf n) (Leaf (n + 1))

-- Replaces all leaves with t with sprout containing leaves
-- t and t + 1
spring :: Bush Int -> Bush Int
spring t = t >>= sprout
```

Using the definition of `liftM2`, we can get intuition for how `Bush` is an applicative; `liftA2 f b1 b2` partially applies `f` over every leaf in b1 and then applies the resulting function over `b2`. Since this occurs at every leaf in `b1`, the 'mapped over' b2 is grafted into every leaf of `b1`.
## Monad laws

<center> <strong>First monad law:</strong> <code> pure x &gt;&gt;= f = f x</code> </center>

<center> <strong>Second monad law:</strong> <code> mx &gt;&gt;= pure = mx</code> </center>

<center> <strong>Third monad law:</strong> <code> (mx &gt;&gt;= f) &gt;&gt;= g = mx &gt;&gt;= (\x -&gt; f x &gt;&gt;= g)</code> </center>

The first and second laws state that pure shouldn't have an effect on the structure of the computation. The third law states that bind should be associative, although this is not very clear because of the way that `(>>=)`. To make things easier, we use the `(>=>)` operator

```Haskell
(>=>) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(f >=> g) x = g x >>= f
```

so the third law can be restated as follows

<center> <strong>Third monad law:</strong> <code> (f &gt;=&gt; g) &gt;=&gt; h = f &gt;=&gt; (g &gt;=&gt; h) </code> </center>

```
((f >=> g) >=> h) x = h x >>= (f >=> g)
                = h x >>= (\x -> g x >>= f)
                = (h x >>= g) >>= f
                = ((g >=> h) x) >>= f
                = (f >=> (g >=> h)) x
```

## Comparing functors, applicatives and monads
Using the flipped version of bind `(=<<) = flip (>>=)`, we can clearly see the relationship between functors, applicatives and monads

```Haskell
(=<<) :: Monad m       => (a -> m b) -> m a -> m b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<$>) :: Functor f     =>   (a -> b) -> f a -> f b
($)   ::                    (a -> b) -> a   -> b
```

`Functor` is used to apply a function over a structure
`Applicative` is used to apply a function lifted in a context over another structure in a multiplicative way
`Monad` is used to apply a function that returns a context to a value in a context
## Sequencing of effects
One consequence of lists being non-deterministic is known as **sequencing of effects**. For example, if we wish to multiply every element in one list with every other element in the other list:
```Haskell
(*) <$> [2, 3] <*> [4, 5] -- = [8, 10, 12, 15]
```
the way in which the lists are sequenced gives a different output. This is a result of a list being a non-commutative applicative, where a commutative applicative satisfies
```Haskell
f <$> u <*> v = flip f <$> v <*> u
```

Hence if we run
```Haskell
flip (*) <$> [4, 5] <*> [2, 3] -- = [8, 12, 10, 15]
```

we get a different result.

Two useful applicative operators are also `<*` and `*>` which have the following definitions
```Haskell
(*>) :: Applicative f => f a -> f b -> f b
(*>) mx my = liftA2 (\x y -> y) mx my

(<*) :: Applicative f => f a -> f b -> f a
(<*) mx my = liftA2 (\x y -> x) mx my
```

`(*>)` combines effects while preserving only the values of its second argument, while `(<*)` combines effects while preserving only the value of the first argument. These operations are particularly useful when we want to carry out two computations but we only care about the result of one of them (for example in parser combinators).
# Important monads
## IO
`IO a` is a type that abstracts programs of type `a` that interact with IO operations and therefore have side effects. Important functions associated with `IO` are
- `putStrLn :: String -> IO ()`
- `print :: Show a => a -> IO ()`
- `writeFile :: FilePath -> String -> IO ()`
- `getChar :: IO Char`
- `getLine :: IO String`
- `readFile :: FilePath -> IO String`

`IO ()` is a program which returns the unit type i.e. a program with no result. Notice how `getChar`, `getLine` and `readFile` only return values wrapped in `IO`. Since IO values represent side effects, this ensures that they can only be operated on using functors, applicatives and monads, shielding them from the rest of the program.

`Functor` instance of `IO:

```Haskell
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```

`Applicative` instance of `IO`:

```Haskell
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
```

The action that a `do` block produces will always have the result value of its last action. This allows us to easily input a string, reverse it and output the result:

```Haskell
main = do line <- fmap reverse getLine
          putStrLn line
```

## `Alternative`

`Alternative` is used for amalgamating results of multiple computations. Its typeclass is defined as follows
```Haskell
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
```

`empty` represents an applicative computation with zero results, while `(<|>)` is a binary function which combines two computations.

For example,
```Haskell
instance Alternative Maybe where
    empty               = Nothing
    Nothing <|> Nothing = Nothing
    Just x <|> Nothing  = Just x
    Nothing <|> Just x  = Just x
    Just x <|> Just y   = Just x -- we choose to discard 2nd result

instance Alternative [] where
    empty = []
    (<|>) = (++)
```

`Alternative` can be used for parallel parsing, allowing a second parser to run if the first parser fails.

## `Writer`

`Writer` is defined as follows (simplified definition)
```Haskell
newtype Writer w a = Writer { runWriter :: (a, w) }
```

It is a monad instance as below
```Haskell
instance (Monoid w) => Monad (Writer w) where
    return x = writer (x, mempty)

    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in writer (y, v `mappend` v')
```

The `Writer` monad contains a value attached to a monoid; the monoid acts as a 'log' of previous computations. From the definition of bind for `Writer`, it is clear how this is done; `x` is replaced with a new value `y` but the previous log is combined with the new log. From the newtype definition, we see that `runWriter` unwraps the writer computation as a `(result, output)` pair. 

With this definition, we can do some interesting operations, for example
```Haskell
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, [show x])

multWithLog :: Writer [String] Int
multWithLog x y = do
    a <- logNumber x
    b <- logNumber y
    return (a * b)
```

`runWriter (multWithLog 3 5)`, for example, will output `(15, ["3", "5"])`.  

`Writer` is itself an instance of the `MonadWriter` typeclass. It includes a number of useful functions, including 
- `writer :: (Monoid w, Monad m) => (a, w) -> m a` - embeds a given log and value in the `Writer` monad
- `tell :: Monoid w => w -> Writer w ()` - allows us to update the log without affecting the current value. For example, in `multWithLog` we may want to indicate when the multiplication has terminated
```Haskell
    multWithLog :: Writer [String] Int
    multWithLog x y = do
        a <- logNumber x
        b <- logNumber y
        tell ["Finished"]
        return (a * b)
    ```
    so `runWriter (multWithLog 3 5)` will output `(15, ["3", "5", "Finished"])`
- `runWriter :: Writer w a -> (a, w)` - unwraps writer computation as `(result, log)` pair
- `execWriter :: Writer w a -> w` - extracts the output from a writer computation

It is important to consider the monoid used in `Writer`; for example, `mappend` for lists involves concatenating the lists together, which is very inefficient if `(++)` associates to the left. In these situation, difference lists can be used instead, as they support efficient appending to other lists.

## `Reader`
The `Reader` monad is used for read-only values that need to be passed throughout function calls. It is defined as follows

```Haskell
data Reader cfg a = Reader { runReader :: cfg -> a }
```
`cfg` represents the static configuration that we are passing, and `a` is the result of the computation that uses the config.

## `State`
The `State` monad is used for threading state through functions in a functionally pure way. In general, a stateful computation is a function that takes some state and returns a value along with some new state. Such a function has the type `s -> (a, s)`, where `s` is the type of the state and `a` is the result of the stateful computations.

`State` is a newtype which is defined as follows

```Haskell
newtype State s a = State { runState :: s -> (a, s) }
```

`State` is an instance of `Monad` as follows

```Haskell
instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState
```
