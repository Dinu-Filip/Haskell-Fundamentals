import Data.List (delete)

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node v r bh) = v

rank :: BinTree a -> Int
rank (Node v r bh) = r

children :: BinTree a -> [BinTree a]
children (Node v r bh) = bh

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees bt1 bt2 = Node (value smallT) 
                            (rank smallT + 1) 
                            (largeT : children smallT)
  where smallT = min bt1 bt2
        largeT = max bt1 bt2

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin = minimum . map value

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h []      = h
mergeHeaps [] h      = h
mergeHeaps h@(t : ts) h'@(t' : ts')
  | rank t < rank t' = t : mergeHeaps ts h'
  | rank t' < rank t = t' : mergeHeaps ts' h
  | otherwise        = mergeHeaps (mergeHeaps ts ts') [combineTrees t t']

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert x = mergeHeaps [Node x 0 []]

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin bh = mergeHeaps (delete minTree bh) ((reverse . children) minTree)

  where minTree = (head . filter ((== extractMin bh) . value)) bh

binSort :: Ord a => [a] -> [a]
binSort xs = binSort' (foldr insert [] xs)

  where binSort' :: Ord a => BinHeap a -> [a]
        binSort' [] = []
        binSort' bh = extractMin bh : binSort' (deleteMin bh)


--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary bh = toBinary' bh [] [0..]
  where toBinary' :: BinHeap a -> [Int] -> [Int] -> [Int]
        toBinary' [] bs _ = bs
        toBinary' (bt : bts) bs (r : rs)
          | rank bt == r = toBinary' bts (1 : bs) rs
          | otherwise    = toBinary' (bt : bts) (0 : bs) rs

binarySum :: [Int] -> [Int] -> [Int]
binarySum xs ys = dropWhile (== 0) (cout : res)

  where lengthDiff = abs (length xs - length ys)
        leading0s = replicate lengthDiff 0
        xs' = if length xs < length ys then leading0s ++ xs else xs
        ys' = if length ys < length xs then leading0s ++ ys else ys
        (cout, res) = binarySum' xs' ys'

        binarySum' :: [Int] -> [Int] -> (Int, [Int])
        binarySum' [] [] = (0, [])
        binarySum' (x : xs) (y : ys) = (decSum `div` 2, decSum `mod` 2 : rest)

          where (cin, rest) = binarySum' xs ys
                decSum = cin + x + y

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]



