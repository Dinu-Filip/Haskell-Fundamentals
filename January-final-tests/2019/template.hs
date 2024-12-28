module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp x = fromJust . lookup x

-- 3 marks
-- Could use helper function to avoid (nub . sort) repetition
vars :: Formula -> [Id]
vars (Var id)    = [id]
vars (Not f)     = (nub . sort) (vars f)
vars (And f1 f2) = (nub . sort) (vars f1 ++ vars f2)
vars (Or f1 f2)  = (nub . sort) (vars f1 ++ vars f2)

-- 1 mark
idMap :: Formula -> IdMap
idMap f = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Not f))     = toNNF f
toNNF (Not (And f1 f2)) = Or (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Or f1 f2))  = And (toNNF (Not f1)) (toNNF (Not f2))
toNNF (And f1 f2)       = And (toNNF f1) (toNNF f2)
toNNF (Or f1 f2)        = Or (toNNF f1) (toNNF f2)
toNNF f                 = f

-- 3 marks
toCNF :: Formula -> CNF
toCNF f = goCNF (toNNF f)
  where goCNF :: Formula -> CNF
        goCNF (And f1 f2) = And (toCNF f1) (toCNF f2)
        goCNF (Or f1 f2)  = distribute (toCNF f1) (toCNF f2)
        goCNF f           = f


-- 4 marks
flatten :: CNF -> CNFRep
flatten f = goFlatten f
  where goFlatten :: CNF -> CNFRep
        goFlatten (Var id)        = [[lookUp id ids']]
        goFlatten (Not (Var id))  = [[- lookUp id ids']]
        goFlatten (Or f1 f2)      = [foldl1 (++) (goFlatten f1 ++ goFlatten f2)]
        goFlatten (And f1 f2)     = goFlatten f1 ++ goFlatten f2

        ids' = idMap f

--------------------------------------------------------------------------
-- Part III

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits rep
  | null units = (rep, [])
  | otherwise = (finalRep, unit : rest)

  where units = filter isSingleton rep
        [unit] = head units
        popRep = [delete (- unit) c | c <- rep, unit `notElem` c]
        (finalRep, rest) = propUnits popRep

-- 4 marks
dp :: CNFRep -> [[Int]]
dp [] = []
dp rep
  | null rep'       = [props]
  | [] `elem` rep'  = []
  | otherwise       = map (props ++) (dp ([next] : rep') ++ dp ([-next] : rep'))

  where (rep', props) = propUnits rep
        next = (head . head) rep'



--------------------------------------------------------------------------
-- Part IV

singleSat :: [(Id, Int)] -> [Id] -> [Int] -> [[(Id, Bool)]]
singleSat _ [] _ = [[]]
singleSat ids (v : vs) is
  | idx `elem` is = map ((v, True) :) (singleSat ids vs is')
  | -idx `elem` is = map ((v, False) :) (singleSat ids vs is'')
  | otherwise = map ((v, True) :) rest ++ map ((v, False) :) rest

  where idx = lookUp v ids
        is' = delete idx is
        is'' = delete (-idx) is
        rest = singleSat ids vs is

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat f = concatMap (singleSat ids allVars) assigns
  where assigns = (dp . flatten . toCNF) f
        allVars = vars f
        ids = idMap f
                
        

        


