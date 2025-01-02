{-# LANGUAGE BangPatterns #-}

import Control.Applicative

-- Exercise 1, 2, 3, 4

toDigits :: Integer -> [Integer]
toDigits n
    | n == 0    = []
    | otherwise = toDigits' n []

    where toDigits' :: Integer -> [Integer] -> [Integer]
          toDigits' 0 !digits = digits
          toDigits' n digits  = toDigits' (n `div` 10) (n `mod` 10 : digits)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns = getZipList ((*) <$> ZipList (cycle [1, 2]) <*> ZipList ns)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a c b = [(a, b)]
hanoi n a c b = hanoi (n - 1) a b c ++ [(a, b)] ++ hanoi (n - 1) c a b

-- Exercise 6

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 1 a b c d = [(a, d)]
hanoi' n a b c d = hanoi' ((n - 1) `div` 2) a c d b ++ hanoi ((n - 1) - ((n - 1) `div` 2)) a d c ++ [(a, d)]
                    ++ hanoi ((n - 1) - ((n - 1) `div` 2)) c a d ++ hanoi' ((n - 1) `div` 2) b a c d