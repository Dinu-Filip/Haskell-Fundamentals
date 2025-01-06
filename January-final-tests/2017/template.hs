import Data.Maybe
import Data.List
import Data.Function (on)

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame = (<= 1) . length . nub

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x = filter ((/= x) . fst)

attIdx :: AttName -> Header -> Int
attIdx att header = (fromJust . elemIndex att) (map fst header)

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt att header r = r !! attIdx att header

removeAtt :: AttName -> Header -> Row -> Row
removeAtt att header r = r1 ++ r2
  where (r1, _ : r2) = splitAt (attIdx att header) r

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (x, y) [] = [(x, [y])]
addToMapping (x, y) ((x', ys') : xys)
  | x == x' = (x, y : ys') : xys
  | otherwise = (x', ys') : addToMapping (x, y) xys

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (_, att) (_, ds) = map (countAtt ds) att

countAtt :: [Row] -> AttValue -> (AttValue, Int)
countAtt ds v = (v, foldr (\r n -> if v `elem` r then n + 1 else n) 0 ds)

--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes Null         = 0
nodes (Leaf _)     = 1
nodes (Node _ dts) = 1 + sum (map (nodes . snd) dts)

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _                  = ""
evalTree (Leaf v) _ _              = v
evalTree (Node attName subDts) h r = evalTree (lookUp (lookUpAtt attName h r) 
                                                       subDts) h r

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (header, rows) (name, vals) 
  = map (\(val, rows) -> (val, (remove name header, rows))) 
        (foldr (\row ps -> addToMapping (lookUpAtt name header row, 
                                         removeAtt name header row) ps) 
               (map (, []) vals) rows)

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree ds@(header, rows) c@(cName, _) selector
  | null rows                                   
      = Null
  | allSame (map (lookUpAtt cName header) rows) 
      = Leaf (lookUpAtt cName header (head rows))
  | otherwise 
      = Node attName (map (\(val, ds) -> (val, buildTree ds c selector)) 
                     (partitionData ds att))

  where att@(attName, _) = selector ds c
  

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

entropy :: DataSet -> Attribute -> Double
entropy (_, []) _      = 0
entropy ds@(_, rs) att = (sum . map (negate . xlogx . 
                         (/ fromIntegral (length rs)) . fromIntegral . snd)) 
                         (buildFrequencyTable att ds)

gain :: DataSet -> Attribute -> Attribute -> Double
gain ds@(header, rows) att c
  = entropy ds c - sum (zipWith (*) probs es)

  where freqs = buildFrequencyTable att ds
        probs = map ((/ fromIntegral (length rows)) . fromIntegral . snd) freqs
        ps = partitionData ds att
        es = map (flip entropy c . snd) ps

bestGainAtt :: AttSelector
bestGainAtt ds@(header, row) c 
  = snd (maximumBy (compare `on` fst) 
        [(gain ds att c, att) | att <- header, att /= c])

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]
