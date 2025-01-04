import Data.List (tails, init, maximumBy)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix s1 s2 = length s1 <= length s2 && and (zipWith (==) s1 s2)

removePrefix :: String -> String -> String
-- Pre - first string prefix of second
removePrefix s1 s2 = (snd . splitAt (length s1)) s2

suffixes :: [a] -> [[a]]
suffixes s = takeWhile (not . null) (iterate tail s)

isSubstring :: String -> String -> Bool
isSubstring s1 s2 = any (isPrefix s1) (suffixes s2)

findSubstrings :: String -> String -> [Int]
findSubstrings s1 s2 = [idx | (idx, suff) <- [0..] `zip` suffixes s2,
                              isPrefix s1 suff]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf n) = [n]
getIndices (Node ts) = concatMap (getIndices . snd) ts

partition' :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition' (x : xs) (y : ys)
  | x == y    = (x : p, xs', ys')
  | otherwise = ([], x : xs, y : ys)

  where (p, xs', ys') = partition' xs ys
partition' xs ys = ([], xs, ys)

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Leaf n)
  | null s    = [n]
  | otherwise = []
findSubstrings' s (Node st) = concatMap (findSingleSubstring s) st

  where findSingleSubstring :: String -> (String, SuffixTree) -> [Int]
        findSingleSubstring s (a, st)
          | null s'      = getIndices st
          | null a'      = findSubstrings' s' st
          | otherwise    = []

          where (p, s', a') = partition' s a

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node []) = Node [(s, Leaf n)]
insert (s, n) (Node ((a, t) : rest))
  | null a'      = Node ((a, insert (s', n) t) : rest)
  | not (null p) = Node ((p, Node [(s', Leaf n), (a', t)]) : rest)
  | otherwise    = Node ((a, t) : restInsert)

  where (p, s', a') = partition' s a
        (Node restInsert) = insert (s, n) (Node rest)

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

-- Finds longest substring of path from root to node with only leaves as
-- children
-- We only need to consider the label of each subtree for the *longest* repeated
-- substring

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring st
  = maximumBy (\x y -> compare (length x) (length y)) (lrs "" ("", st))

  where lrs :: String -> (String, SuffixTree) -> [String]
        lrs s (_, Leaf _) = []
        lrs s (a, Node ts)
          | null subs = [s ++ a]
          | otherwise = subs

            where subs = concatMap (lrs (s ++ a)) ts


------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


