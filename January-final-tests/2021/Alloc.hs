module Alloc where

import Data.Maybe
import Data.List
import Data.Bool

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x = foldl (\c y -> if y == x then c + 1 else c) 0

degrees :: Eq a => Graph a -> [(a, Int)]
degrees g = [(v, count v endpts) | v <- fst g]
    -- Gets endpoints of all arcs in graph
    where endpts = concatMap (\(x, y) -> [x, y]) (snd g)

neighbours :: Eq a => a -> Graph a -> [a]
neighbours v g = map (\(x, y) -> if x == v then y else x) adjEdges
    -- Filters all edges with vertex as endpoint
    where adjEdges = filter (\e -> fst e == v || snd e == v) (snd g)

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n g = (restNodes, restEdges)
    -- Gets all remaining nodes
    where restNodes = filter (/= n) (fst g)
          -- Filters edges without n as endpoint
          restEdges = filter (\e -> not $ fst e == n || snd e == n) (snd g) 

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
-- c >= 1 so if only one node, then node given minimum colouring
colourGraph _ ([x], _) = [(x, 1)]
colourGraph c g@(xs, es) = (n, nC) : cMap
    where gSorted = sortGraph g
          degs = degrees gSorted
          -- Gets node with smallest degree
          (n, _) = foldl1 (\e1@(_, d1) e2@(_, d2) -> if d1 <= d2 
                                                     then e1 
                                                     else e2) degs
          -- Graph with n removed
          g' = removeNode n gSorted
          -- Colouring of remaining graph
          cMap = colourGraph c g'
          -- Gets colours of remaining nodes
          restCs = map (`lookUp` cMap) (neighbours n gSorted)
          -- Finds smallest colour not taken by neighbours
          -- If none found then left uncoloured (0)
          nC = let cs = [1..c] \\ restCs in if not $ null cs 
                                            then head cs 
                                            else 0


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
-- If self assign then line omitted
renameBlock b idMap = filter (not . isSelfAssign) $ map (`renameStatement` idMap) b
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
-- Filters only live variables i.e. lines with at least two variables
buildIG vars = (nub $ concat vars, 
                nub $ map (\[x, y] -> (x, y)) $ filter ((== 2) . length) vars)

-----------------------------------------------------
--
-- Part V
--

liveVars :: CFG -> [[Id]]
-- Creates initial set of live vars
liveVars cfg = go (replicate (length cfg) [])
    where go :: [[Id]] -> [[Id]]
          -- Iterates until liveVar set doesn't change
          go prev = if next == prev then next else go next
            where next = map nLiveVar cfg
                  nLiveVar :: ((Id, [Id]), [Int]) -> [Id]
                  -- Applies formula in spec
                  nLiveVar ((def, use), succs) = nub $ use ++ concatMap (filter (/= def) . (prev !!)) succs

-- Retrieves variables in expression
getExpVars :: Exp -> [Id]
getExpVars (Var id)        = [id]
getExpVars (Apply _ e1 e2) = getExpVars e1 ++ getExpVars e2
getExpVars _               = []

buildCFG :: Function -> CFG
buildCFG (_, _, b) = go b 0
    where go :: Block -> Int -> CFG
          go [] _ = []
          -- If return then no pointer to next line
          go ((Assign "return" exp) : rest) n = (("return", getExpVars exp), []) : go rest (n + 1)
          -- If last line in program then no next pointer
          go [Assign id exp] n = [((id, getExpVars exp), [])]
          go ((Assign id exp) : rest) n = ((id, getExpVars exp), [n + 1]) : go rest (n + 1)
          go ((If exp b1 b2) : rest) n = ((("_", getExpVars exp), [n + 1, elseIdx]) : cfg1 ++ cfg2) ++ go rest afterIdx
            where cfg1 = go b1 (n + 1)
                  cfg2 = go b2 elseIdx
                  -- Starting line of else block
                  elseIdx = n + 1 + length cfg1
                  -- Starting line after all if statement
                  afterIdx = elseIdx + length cfg2
          go ((While exp b) : rest) n = ((("_", getExpVars exp), [n + 1, afterIdx]) : cfg') ++ go rest afterIdx
            where cfg = go b (n + 1)
                  (line, _) = last cfg
                  cfg' = init cfg ++ [(line, [n])]
                  afterIdx = n + 1 + length cfg'
