import Data.Maybe

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show)

showT :: Type -> String
showT TInt  
  = "Int"
showT TBool 
  = "Bool"
showT (TFun t t') 
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a) 
  = a
showT TErr  
  = "Type error"

type TypeTable = [(String, Type)]

type TEnv 
  = TypeTable    -- i.e. [(String, Type)]

type Sub 
  = TypeTable    -- i.e. [(String, Type)]  

-- Built-in function types...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

------------------------------------------------------
-- PART I

-- Pre: The search item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x = fromJust . lookup x

tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp x y [] = y
tryToLookUp x y ((x', y') : xys)
  | x == x'   = y'
  | otherwise = tryToLookUp x y xys

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp y xys = [x | (x, y') <- xys, y == y']

occurs :: String -> Type -> Bool
occurs s (TVar t)     = t == s
occurs s (TFun t1 t2) = occurs s t1 || occurs s t2
occurs _ _            = False

------------------------------------------------------
-- PART II

-- Pre: There are no user-defined functions (constructor Fun)
-- Pre: All variables in the expression have a binding in the given 
--      type environment
inferType :: Expr -> TEnv -> Type
inferType (Number _) _               = TInt
inferType (Boolean _) _              = TBool
inferType (Id s) tEnv                = lookUp s tEnv
inferType (Prim s) _                 = lookUp s primTypes
inferType (Cond c1 e1 e2) tEnv
  | condT == TBool && thenT == elseT = thenT
  | otherwise                        = TErr

  where condT = inferType c1 tEnv
        thenT = inferType e1 tEnv
        elseT = inferType e2 tEnv
inferType (App f e) tEnv
  | (TFun t t') <- ft, t == et       = t'
  | otherwise                        = TErr

  where ft = inferType f tEnv
        et = inferType e tEnv

------------------------------------------------------
-- PART III

applySub :: Sub -> Type -> Type
applySub _ TInt        = TInt
applySub _ TBool       = TBool
applySub s t@(TVar v)  = tryToLookUp v t s
applySub s (TFun t t') = TFun (applySub s t) (applySub s t')

unify :: Type -> Type -> Maybe Sub
unify t t'
  = unifyPairs [(t, t')] []

unifySingleVar :: String -> Type -> Sub -> [(Type, Type)] -> Maybe Sub
unifySingleVar v t s ts
  | occurs v t = Nothing
  | otherwise  = unifyPairs ts' ((v, t) : s)

  where ts' = [(applySub [(v, t)] t1, applySub [(v, t)] t2) | (t1, t2) <- ts]

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] s                    = Just s
unifyPairs ((TInt, TInt) : ts) s   = unifyPairs ts s
unifyPairs ((TBool, TBool) : ts) s = unifyPairs ts s
unifyPairs ((TVar v, TVar v') : ts) s
  | v == v'                        = unifyPairs ts s
  | otherwise                      = Nothing
unifyPairs ((TVar v, t) : ts) s    = unifySingleVar v t s ts
unifyPairs ((t, TVar v) : ts) s    = unifySingleVar v t s ts
unifyPairs ((TFun t1 t2, TFun t1' t2') : ts) s 
                                   = unifyPairs ((t1, t1') : (t2, t2') : ts) s
unifyPairs _ _                     = Nothing

------------------------------------------------------
-- PART IV

updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub
  = map modify tenv
  where
    modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld
  = sNew ++ updateTEnv sOld sNew

-- In combineSubs [s1, s2,..., sn], s1 should be the *most recent* substitution
-- and will be applied *last*
combineSubs :: [Sub] -> Sub
combineSubs 
  = foldr1 combine

inferPolyType :: Expr -> Type
inferPolyType e = let (_, t, _) = inferPolyType' e [] 1 in t

-- You may optionally wish to use one of the following helper function declarations
-- as suggested in the specification. 

-- inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
-- inferPolyType'
--   = undefined

inferPolyType' :: Expr -> TEnv -> Int -> (Sub, Type, Int)
inferPolyType' (Number _) _ n  = ([], TInt, n)
inferPolyType' (Id s) tEnv n   = ([], lookUp s tEnv, n)
inferPolyType' (Boolean _) _ n = ([], TBool, n)
inferPolyType' (Prim s) _ n    = ([], lookUp s primTypes, n)
inferPolyType' (Fun x e) tEnv n
  | te == TErr                 = (s, TErr, m)
  | otherwise                  = (s, TFun a' te, m)
  
  where a = TVar ("a" ++ show n)
        tEnv' = (x, a) : tEnv
        (s, te, m) = inferPolyType' e tEnv' (n + 1)
        a' = applySub s a
inferPolyType' (App f e) tEnv n
  | isNothing uft              = ([], TErr, n)
  | otherwise                  = (s', a', k + 1)

  where (fs, tf, m) = inferPolyType' f tEnv n
        tEnv' = updateTEnv tEnv fs
        (es, te, k) = inferPolyType' e tEnv' m
        a = TVar ("a" ++ show k)
        uft = unify tf (TFun te a)
        s' = combineSubs [fromJust uft, es, fs]
        a' = applySub (fromJust uft) a
inferPolyType' (Cond c e1 e2) tEnv n
  | tc /= TBool                = ([], TErr, n)
  | isNothing uft              = ([], TErr, n)
  | otherwise                  = (s', applySub s' te1, l)

  where (cs, tc, m) = inferPolyType' c tEnv n
        tEnv' = updateTEnv tEnv cs
        (e1s, te1, k) = inferPolyType' e1 tEnv m
        tEnv'' = updateTEnv tEnv' e1s
        (e2s, te2, l) = inferPolyType' e2 tEnv' k
        tEnv''' = updateTEnv tEnv'' e2s
        uft = unify te1 te2
        s' = combineSubs [fromJust uft, e2s, e1s, cs]


------------------------------------------------------
-- Monomorphic type inference test cases from Table 1...

env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Unification test cases from Table 2...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Polymorphic type inference test cases from Table 3...

ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))

