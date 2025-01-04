import Data.List
import Data.Maybe
import Data.Binary.Get (Decoder(Fail))
type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp = (fromJust .) . lookup

checkSat :: BDD -> Env -> Bool
checkSat (curr, nodes) env = case nextNode of
  1 -> True
  0 -> False
  _ -> checkSat (nextNode, nodes) env

  where (v, l, r) = lookUp curr nodes
        nextNode = if lookUp v env then r else l

sat :: BDD -> [[(Index, Bool)]]
sat (0, _) = []
sat (1, _) = [[]]
sat (curr, nodes) = map ((idx, False) :) (sat (lt, nodes)) ++ 
                    map ((idx, True) :) (sat (rt, nodes))

  where (idx, lt, rt) = lookUp curr nodes

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim b))            = Prim (not b)
simplify (And (Prim b1) (Prim b2)) = Prim (b1 && b2)
simplify (Or (Prim b1) (Prim b2))  = Prim (b1 || b2)
simplify e                         = e

restrict :: BExp -> Index -> Bool -> BExp
restrict e@(Prim _) _ _    = e
restrict e@(IdRef r) idx b
  | r == idx               = Prim b
  | otherwise              = e
restrict (Not e) idx b     = simplify $ Not (restrict e idx b)
restrict (And e1 e2) idx b = simplify $ And (restrict e1 idx b) 
                                            (restrict e2 idx b)
restrict (Or e1 e2) idx b  = simplify $ Or (restrict e1 idx b) 
                                          (restrict e2 idx b)

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD be = buildBDD' be 2

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
getNextChild :: (NodeId, [BDDNode]) -> NodeId
getNextChild (nid, nodes)
  | null nodes = nid
  | otherwise = if ln == rn then ln else nid

    where (_, ln, rn) = lookUp nid nodes

buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' e _ [] = case e of
  Prim True  -> (1, [])
  Prim False -> (0, [])
buildBDD' e nodeId (i : is) = (nodeId, (nodeId, (i, id1, id2)) : ns1 ++ ns2)
  where (id1, ns1) = buildBDD' (restrict e i False) (2 * nodeId) is
        (id2, ns2) = buildBDD' (restrict e i True) (2 * nodeId + 1) is
        


------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e ids = buildROBDD' e 2 ids (2, [])

reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp y xys = [x | (x, y') <- xys, y' == y]

-- Tail recursion makes second implementation easier
-- root represents the last node that was added and nodeId represents the next
-- node to be added
buildROBDD' :: BExp -> NodeId -> [Index] -> BDD -> BDD
buildROBDD' (Prim True) _ [] (_, nodes)      = (1, nodes)
buildROBDD' (Prim False) _ [] (_, nodes)     = (0, nodes)
buildROBDD' e nodeId (i : is) (root, nodes)
  | lNode == rNode                           = (lNode, nodes'')
  | [nodeId'] <- reverseLookUp newNode nodes = (nodeId', nodes'')
  | otherwise                                = (nodeId, 
                                                (nodeId, newNode) : nodes'')

  where fRestrict = restrict e i False
        tRestrict = restrict e i True
        (lNode, nodes')  = 
          buildROBDD' fRestrict (2 * nodeId) is (nodeId, nodes)
        -- Nodes from building left subtree must be passed to right subtree
        (rNode, nodes'') = 
          buildROBDD' tRestrict (2 * nodeId + 1) is (nodeId, nodes')
        newNode = (i, lNode, rNode)

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))
b9 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


