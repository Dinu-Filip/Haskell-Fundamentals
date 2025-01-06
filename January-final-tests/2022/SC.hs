module SC where

import Data.List
import Data.Maybe

import Types
import Examples

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I

isFun :: Exp -> Bool
isFun (Fun _ _) = True
isFun _         = False

splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs bs 
  = foldl (\(func, vars) b@(_, e) -> 
              if isFun e 
              then (b : func, vars) 
              else (func, b : vars))
          ([], []) bs


topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bs _) = (length . fst . splitDefs) bs

---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll = nub . concat

freeVars :: Exp -> [Id]
freeVars (Const _)   = []
freeVars (Var v)
  | v `elem` prims = []
  | otherwise      = [v]
freeVars (App e es)  = (unionAll . map freeVars) (e : es)
freeVars (Fun ids e) = freeVars e \\ ids
freeVars (Let bs e)  = unionAll (freeVars e : map freeVars boundEs) \\ boundIds
  where (boundIds, boundEs) = unzip bs


---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap (Let bs e) 
  = [(f, freeVars) | f <- names, 
                     let freeVars = sort ((unionAll allFreeVars) \\ names)]
    ++ nextEs ++ buildFVMap e

  where (fs, vs) = splitDefs bs
        (names, defs) = unzip fs
        allFreeVars = map freeVars defs
        nextEs = concatMap (\(Fun _ fe) -> buildFVMap fe) defs
buildFVMap _          = []

modifyBinding :: [(Id, [Id])] -> Binding -> Binding
modifyBinding fVars (f, Fun as e) = ('$' : f, Fun ((lookUp f fVars) ++ as) 
                                                  (modifyFunctions fVars e))
modifyBinding fVars (v, e)        = (v, modifyFunctions fVars e)

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions _ (Const x) = Const x
modifyFunctions fVars (Var x)
  | x `elem` map fst fVars  = let vs = lookUp x fVars in case vs of
      [] -> Var ('$' : x)
      _  -> App (Var ('$' : x)) (map Var vs)
  | otherwise = Var x
modifyFunctions fVars (App e es) = App (modifyFunctions fVars e) 
                                       (map (modifyFunctions fVars) es)
modifyFunctions fVars (Let bs d) = Let (map (modifyBinding fVars) bs) 
                                        (modifyFunctions fVars d)

-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift e = Let scs e'
  where (e', scs) = lift' e

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' (Let bs e)
  | null vs = (e', fs ++ scs)
  | otherwise = (Let vs e', fs ++ scs)
  where (fs, vs) = splitDefs bs
        (e', scs) = lift' e
lift' e = (e, [])