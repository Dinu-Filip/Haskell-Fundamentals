import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp x = fromJust . lookup x

states :: LTS -> [State]
states = nub . concatMap (\((x, y), _) -> [x, y])

transitions :: State -> LTS -> [Transition]
transitions s = filter (\((s', _), _) -> s' == s)

alphabet :: LTS -> Alphabet
alphabet = nub . map snd

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions = nub . actions'
  where actions' :: Process -> [Id]
        actions' STOP                = []
        actions' (Ref _)             = []
        actions' (Prefix id process) = id : actions process
        actions' (Choice ps)         = concatMap actions ps

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts ids pDefs@((start, pDef) : rest) = accepts' ids pDef
  where accepts' :: [Id] -> Process -> Bool
        accepts' [] _                    = True
        accepts' ids (Ref pId)           = accepts' ids (lookUp pId pDefs)
        accepts' (i : is) (Prefix id' p) = id' == i && accepts' is p
        accepts' ids (Choice ps)         = any (accepts' ids) ps

------------------------------------------------------
-- PART III

--composeTransitions :: Transition -> Transition 
--                   -> Alphabet -> Alphabet 
--                   -> StateMap 
--                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.

composeTransitions :: Transition -> Transition 
                  -> Alphabet -> Alphabet 
                  -> StateMap 
                  -> [Transition]
composeTransitions ((s, t), a) ((s', t'), a') as1 as2 sMap
  | a == a'                       
      = [((lookUp (s, s') sMap, lookUp (t, t') sMap), a)]
  | a `elem` as2 && a' `elem` as1 = []
  | a' `elem` as1 
      = [((lookUp (s, s') sMap, lookUp (t, s') sMap), a)]
  | a `elem` as2                  
      = [((lookUp (s, s') sMap, lookUp (s, t') sMap), a')]
  | otherwise 
      = [((lookUp (s, s') sMap, lookUp (t, s') sMap), a), 
         ((lookUp (s, s') sMap, lookUp (s, t') sMap), a')]

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts = visit 0 []

  where visit :: State -> [State] -> [Transition]
        visit s visited
          | s `notElem` visited = tsFromS ++ 
              concatMap (flip visit (s : visited) . (snd . fst)) tsFromS
          | otherwise = []

          where tsFromS = transitions s ts


------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose lts1 lts2 = pruneTransitions ts
  where states1 = sort $ states lts1
        states2 = sort $ states lts2
        sts = [(s, s') | s <- states1, s' <- states2]
        sToI = sts `zip` [0..]
        as1 = alphabet lts1
        as2 = alphabet lts2
        ts = concat [composeTransitions t1 t2 as1 as2 sToI | 
                      (s, s') <- sts, 
                      t1 <- transitions s lts1, 
                      t2 <- transitions s' lts2]

------------------------------------------------------
-- PART V

-- Recurses on each process definition
--
-- Given definition, next available state used to represent the process
--
-- State and process name maintained in list
--
-- Each prefix leads to new state
--
-- If state encountered in ref, then corresponding state looked up in
-- accumulator; it is assumed that all refs will have been encountered before
--

reverseLookup :: Eq b => b -> [(a, b)] -> a
reverseLookup x xys = (fst . head) (filter ((x ==) . snd) xys)

generateFromProcess :: Process -> State -> State -> [Transition] 
                    -> [(Id, State)] 
                    -> ([Transition], State, [(Id, State)])
generateFromProcess (Prefix a STOP) prev n ts sMap = 
  (((prev, n), a) : ts, n + 1, sMap)
generateFromProcess (Prefix a (Ref p)) prev n ts sMap = 
  (((prev, lookUp p sMap), a) : ts, n, sMap)
generateFromProcess (Prefix a p) prev n ts sMap = 
  (((prev, n), a) : ts', n + 1, sMap)

  where (ts', n', sMap') = generateFromProcess p n (n + 1) ts sMap
generateFromProcess (Choice ps) prev n ts sMap = 
  foldl (\(ts', n', sMap') p -> 
    generateFromProcess p prev n' ts' sMap') (ts, n, sMap) ps
generateFromProcess (Ref p) prev n ts sMap = (ts, prev, updateMs)

  where updateMs = map (\(d, s) -> 
                    if s == prev then (d, lookUp p sMap) else (d, s)) sMap

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS pDefs = lts
  where (lts, _, _) = foldl (\(ts', s', sMap') (d, p) -> 
                        generateFromProcess p (lookUp d sMap') s' ts' sMap') 
                      ([], length sMap, sMap) pDefs
        sMap = map fst pDefs `zip` [0..]
------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]

