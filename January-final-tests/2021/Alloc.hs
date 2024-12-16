module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x ys = foldl (\c y -> if y == x then c + 1 else c) 0 ys

degrees :: Eq a => Graph a -> [(a, Int)]
degrees g = [(v, count v endpts) | v <- fst g]
    where endpts = concatMap (\(x, y) -> [x, y]) (snd g)

neighbours :: Eq a => a -> Graph a -> [a]
neighbours v g = map (\(x, y) -> if x == v then y else x) adjEdges
    where adjEdges = filter (\e -> fst e == v || snd e == v) (snd g)

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n g = (restNodes, restEdges)
    where restNodes = filter (/= n) (fst g)
          restEdges = filter (\e -> not $ fst e == n || snd e == n) (snd g) 

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([x], _) = [(x, 1)]
colourGraph c g@(xs, es) = (n, nC) : cMap
    where gSorted = sortGraph g
          degs = degrees gSorted
          (n, _) = foldl1 (\e1@(_, d1) e2@(_, d2) -> if d1 <= d2 then e1 else e2) degs
          g' = removeNode n gSorted
          cMap = colourGraph c g'
          restCs = map (flip lookUp cMap) (neighbours n gSorted) 
          nC = head $ ([1..c] \\ restCs) ++ [0]


------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap = (("return", "return") :) . map assignId
    where assignId :: (Id, Colour) -> (Id, Id)
          assignId (v, 0) = (v, v)
          assignId (v, n) = (v, 'R' : show n)

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments ids idMap = map (\id -> Assign (lookUp id idMap) (Var id)) ids

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Var id) idMap = Var (lookUp id idMap)
renameExp (Apply op exp1 exp2) idMap = Apply op (renameExp exp1 idMap) 
                                                (renameExp exp2 idMap)
renameExp exp _ = exp

renameStatement :: Statement -> IdMap -> Statement
renameStatement (Assign id exp) idMap = Assign (lookUp id idMap) 
                                               (renameExp exp idMap)
renameStatement (If exp b1 b2) idMap = If (renameExp exp idMap) 
                                          (renameBlock b1 idMap) 
                                          (renameBlock b2 idMap)
renameStatement (While exp b) idMap = While (renameExp exp idMap) 
                                            (renameBlock b idMap)

renameBlock :: Block -> IdMap -> Block
renameBlock b idMap = filter (not . isSelfAssign) $ map (flip renameStatement idMap) b
    where isSelfAssign :: Statement -> Bool
          isSelfAssign (Assign id (Var id')) = id == id'
          isSelfAssign _                     = False


renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG vars = (nub $ concat vars, nub $ map (\[x, y] -> (x, y)) $ filter ((== 2) . length) vars)

-----------------------------------------------------
--
-- Part V
--

liveVars :: CFG -> [[Id]]
liveVars cfg = go (replicate (length cfg) [])
    where go :: [[Id]] -> [[Id]]
          go prev = if next == prev then next else go next
            where next = map nLiveVar cfg
                  nLiveVar :: ((Id, [Id]), [Int]) -> [Id]
                  nLiveVar ((def, use), succs) = nub $ use ++ filter (/= def) (concatMap (prev !!) succs) 

getExpVars :: Exp -> [Id]
getExpVars (Var id)        = [id]
getExpVars (Apply _ e1 e2) = getExpVars e1 ++ getExpVars e2
getExpVars _               = []

buildCFG :: Function -> CFG
buildCFG (_, _, b) = concatMap (flip go 0) b
    where go :: Statement -> Int -> CFG
          go (Assign "return" exp) n = [(("return", getExpVars exp), [])]
          go (Assign id exp) n = [((id, getExpVars exp), [n + 1])]
          go (If exp b1 b2) n = (("-", getExpVars exp), [n + 1, elseIdx]) : cfg1 ++ cfg2
            where cfg1 = concatMap (flip go (n + 1)) b1
                  cfg2 = concatMap (flip go (elseIdx)) b2
                  elseIdx = n + (length cfg1) + 1
          go (While exp b) n = (("-", getExpVars exp), [n + 1]) : cfg'
            where cfg = concatMap (flip go (n + 1)) b
                  (line, succs) = last cfg
                  cfg' = init cfg ++ [(line, [n])]
