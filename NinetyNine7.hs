module NinetyNine7 where
import qualified Data.List as List hiding (cycle)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Lazy as Map
import NinetyNine1 (combineAll, infinite)

-- Undirected graph representations



data EdgeClause a  = GEC [EC a]      -- Edge-clause form (note that this one accepts single nodes)
  deriving (Eq, Show)
data EC a = Edge (a,a) | Single a
  deriving (Show)
instance (Eq a)=> Eq (EC a) where
  (Edge _) == (Single _)           = False
  (Single _) == (Edge _)           = False
  (Single n1) == (Single n2)       = n1==n2
  (Edge (n1,n2)) == (Edge (n3,n4)) = (n1 == n3 && n2 == n4) ||
                                     (n1 == n4 && n2 == n3)
--



data GraphTerm a     = GTerm ([a],[(a,a)])-- Graph-term form
  deriving (Show)
instance (Eq a)=>Eq (GraphTerm a) where
  (GTerm (nodes1, edges1)) == (GTerm (nodes2, edges2)) =
    (eqSet nodes1 nodes2) && (eqSet (map Edge edges1) (map Edge edges2))
  
data AdjacencyList a = AdjL [(a,[a])]    -- Adjacency-list form
  deriving (Eq, Show)

getNode::(Eq a)=>a->AdjacencyList a->(a,[a])
getNode n (AdjL nodes) = Maybe.fromJust $ List.find (\(n',nes) -> n' == n) nodes

-- Test Graphs
graph701 = GEC [Edge ('h','g'), Edge ('k','f'), Edge ('f', 'b'), Edge ('b','c'),
                Edge ('f','c'), Single 'd']






-- Human Friendly form will be provided only as a method of printing and reading

-- 80

-- (***) Conversions

-- Write predicates to convert between the different graph representations. With 
-- these predicates, all representations are equivalent; i.e. for the following 
-- problems you can always pick freely the most convenient form. The reason this 
-- problem is rated (***) is not because it's particularly difficult, but 
-- because it's a lot of work to deal with all the special cases.

-- Edge-clause to graph-term

ec2gt::(Eq a)=>EdgeClause a->GraphTerm a
ec2gt gec@(GEC ecs) = GTerm (getNodes gec, getEdges ecs)
  where
  getEdges [] = []
  getEdges ((Edge e):es)   = e:(getEdges es)
  getEdges ((Single e):es) = (getEdges es)

getNodes::(Eq a)=>EdgeClause a->[a]
getNodes (GEC ecs) = foldr setConsEdge [] ecs
  where
  setConsEdge (Edge (e1,e2)) xs = setCons e2 $ setCons e1 xs
  setConsEdge (Single e) xs     = setCons e xs

-- Cons, but the list is treated as a set
setCons::(Eq a)=>a->[a]->[a]
setCons x xs
  |elem x xs = xs
  |otherwise = x:xs




--Graph-term to adjacency-list
gt2al::(Eq a)=>GraphTerm a-> AdjacencyList a
gt2al (GTerm (nodes,edges)) = AdjL (map buildNode nodes)
  where
  buildNode x = foldl addEdge (x,[]) edges
  addEdge (x,es) (e1,e2)
    |e1 == x   = (x, e2:es)
    |e2 == x   = (x, e1:es)
    |otherwise = (x,es)




-- Adjacency-list to edge-clause
al2ec::(Eq a)=>AdjacencyList a->EdgeClause a
al2ec (AdjL al) = GEC (foldl addEdge [] al)
  where
  addEdge es (n,[])  = setCons (Single n) es
  addEdge es (n,nes) = List.union es (map (\e -> Edge (n,e)) nes)




-- Graph-term to edge-clause
gt2ec::(Eq a)=>GraphTerm a->EdgeClause a
gt2ec = al2ec.gt2al

-- Edge-clause to adjacency-list
ec2al::(Eq a)=> EdgeClause a-> AdjacencyList a
ec2al = gt2al.ec2gt

-- Human-form to adjacency-list
hf2al = ec2al.hf2ec

-- Adjacency-list to graph-term
al2gt::(Eq a)=>AdjacencyList a->GraphTerm a
al2gt = ec2gt.al2ec

-- Test function that cycles through forms
roundabout::(Eq a)=>EdgeClause a->EdgeClause a
roundabout = al2ec.gt2al.ec2gt




-- Human-friendly to edge-clause. Assumes the node data to be a Char
-- Syntax for human-friendly representation will look like:
-- Graph::=      '['EdgeOrNode(", "EdgeOrNode)*']'
-- EdgeOrNode::= Edge | Node
-- Edge::=       char'-'char
-- Node::=       char
hf2ec::String->EdgeClause Char
hf2ec st = GEC $ hfAux (drop 1 st) []
  where
  hfAux [] es             = es
  hfAux (c1:'-':c2:cs) es = hfAux (drop 2 cs) $ (Edge (c1,c2)):es
  hfAux (c:cs) es         = hfAux (drop 2 cs) $ (Single c):es

ec2hf::EdgeClause Char->String
ec2hf (GEC edges) = '[':(hfAux edges)
  where
  hfAux [e]    = (ecString e)++"]"
  hfAux (e:es) = (ecString e)++", "++(hfAux es)
  ecString (Single c) = [c]
  ecString (Edge (c1,c2)) = [c1,'-',c2]




--test strings:
graph702 = hf2ec "[b-c, f-c, g-h, d, f-b, k-f, h-g]"
graph703 = hf2ec "[a]"
graph704 = hf2ec "[a-b]"
graph705 = hf2ec "[a-b, b-c, c-a]"
graphk4  = hf2ec "[a-b, b-c, c-d, d-a, a-c, b-d]"




-- 81

-- (**) Path from one node to another one

-- Write a function that, given two nodes a and b in a graph, returns all the 
-- acyclic paths from a to b. 


-- pathsAux is a function that may return zero or more paths from start to 
-- goal. it's first argument is a list that holds the path walked through the
-- graph at any point of the recursion. It starts with the start node. It's
-- second argument is the adjacency-list of it's current node. Of course, the
-- first list comes from the start node. For each node in the list, the function
-- will proceed as following:
-- 1. If the node is already present on the list, it means the recursion
--    just closed a cycle and this computation path should therefore be ignored.
-- 2. If the node is the goal, then the path is complete and is returned
-- 3. If it's any other node, said node is put in the path and it's adjacency-list
--    is retrieved. The computation then recursively continues on each of it's
--    adjacent nodes.
paths::(Eq a)=>a->a->AdjacencyList a->[[a]]
paths start goal al = concat $ map (pathsAux [start]) (snd $ getNode start al) 
  where
  pathsAux curPath e 
    |elem e curPath = []
    |e == goal = [reverse $ e:curPath]
    |otherwise = concat $ map (pathsAux $ e:curPath) (snd $ getNode e al)

pathsEC::(Eq a)=>a->a->EdgeClause a->[[a]]
pathsEC start goal ec = paths start goal $ ec2al ec

ec2alApply::(Eq a)=>(AdjacencyList a -> b)->(EdgeClause a -> b)
ec2alApply f = f.ec2al

-- 82
-- (*) Cycle from a given node

-- Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a 
-- given node A in the graph G. The predicate should return all cycles via 
-- backtracking. 

-- I don't understand what the question means by backtracking. The simplest
-- solution is to have the node argument as both the start and goal of
-- a version of pathsAux that accepts cycles. Two cycles [1,2,3,1] and 
-- [1,3,2,1] are considered to be equal, so the list is filtered for repeated 
-- entries using nubBy and setEquals. The list is then also filtered removing 
-- paths with length smaller than 3, because this implies it's just a path
-- along a single edge.

-- Function name changed from cycle to gCycle because the prelude already has
-- a function with the same name.
gCycle::(Eq a)=>a->AdjacencyList a->[[a]]
gCycle node al =
  filter (\xs -> length xs > 3) $
  List.nubBy setEquals $
  concat $ map (cycleAux [node]) (snd $ getNode node al)
  where
  cycleAux curPath e 
    |e == node = [reverse $ e:curPath]
    |elem e curPath = []
    |otherwise = concat $ map (cycleAux $ e:curPath) (snd $ getNode e al)

setEquals::(Eq a)=>[a]->[a]->Bool
setEquals xs ys =
  (length xs == length ys) && (and $ map (\x -> elem x ys) xs)




-- 83

-- (**) Construct all spanning trees

-- Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all 
-- spanning trees of a given graph. With this predicate, find out how many 
-- spanning trees there are for the graph depicted to the left. The data of 
-- this example graph can be found in the file p83.dat. When you have a correct 
-- solution for the s_tree/2 predicate, use it to define two other useful 
-- predicates: is_tree(Graph) and is_connected(Graph). Both are five-minutes 
-- tasks! 

-- (file is not available in the haskell problems website)


-- Spent about 7 hours on this problem but couldn't do it.
-- Maybe I'll come back to it or look for an algorithm online
-- and implement it.
-- The solutions page has a function that generates ALL subgraphs, then
-- selects only the ones that are spanning trees.


type NsEs a   = ((Int, [a]), [(a,a)])
-- This is the shorthand type that accumulates the recursion of spantree
-- ((s, ns), edges)
-- ns = nodes that have already been visited
-- s  = size of the ns list
-- edges = edges that have already been taken

type NodeAL a = (a,[a])
-- shorthand for the adjacency-list node

spantree::(Eq a)=>AdjacencyList a -> [[NsEs a]]
spantree adj@(AdjL al) = spanAux1 ((0,[]),[]) $ head al
  where
  l = length al
  
  --spanAux1::(Eq a)=>NsEs a->NodeAL a->[[NsEs a]]
  spanAux1 acc@((s,ns), edges) (n,nes)
    |null combos = [[acc]] -- this ends the recursion
    |otherwise   = concat results
    where
    combos = combineAll (nes List.\\ ns)
    results = map spanAux2 combos
    
    --spanAux2::(Eq a)=>[a]->[[NsEs a]]
    spanAux2 cs 
      |length cs == 1 = res
      |otherwise      = successes : [combine failures]
      where
      --combine::(Eq a)=>[[NsEs a]]->[NsEs a]
      combine []     = []
      combine [xs]     = xs
      combine (xs:xss) = [ ((s1+s2-s, List.union ns1 ns2), List.union e1 e2) |
                         ((s1,ns1),e1) <- xs,
                         ((s2,ns2),e2) <- combine xss ]
      
      (successes, failures) =
        (foldl f2 ([],[]) $ (map (List.partition ((==l).fst.fst))) res)-- ::([NsEs a],[[NsEs a]])
      f2 (sc,fl) ([],fl2) = (sc,(filterMaxNodes fl2):fl)
      f2 (sc,fl) (sc2, _) = (sc2++sc, fl)
      
      res = concat $ map (\c -> spanAux1 newAcc (getNode c adj)) cs
        where
        newAcc = ((s+length cs, cs++ns), (map (\x -> (n,x)) cs)++edges)
      

filterMaxNodes::[NsEs a]->[NsEs a]
filterMaxNodes nsess =
  [((s,ns),es) | ((s,ns),es) <- nsess, s==maxNodes nsess]
     
maxNodes::[NsEs a]->Int
maxNodes nsess = aux nsess 0
  where
  aux [] n = n
  aux (x:xs) n
    |n' > n    = aux xs n'
    |otherwise = aux xs n
    where
    n' = fst $ fst x
--
--
test701 = spantree (ec2al graph705)
test702 = spantree (ec2al graphk4)


cur  = ((1,"a"),[])::NsEs Char
res1 = ((3,"abc"), [('a','b'),('b','c')])::NsEs Char
res2 = ((4,"abce"),[('a','b'),('b','c'),('b','d')])::NsEs Char
res3 = ((2,"ad"), [('a','d')])::NsEs Char

ls1 = [[res1,res2],[res3]]







-- 84

-- (**) Construct the minimal spanning tree

-- Write a predicate ms_tree(Graph,Tree,Sum) to construct the minimal spanning 
-- tree of a given labelled graph. Hint: Use the algorithm of Prim. A small 
-- modification of the solution of P83 does the trick. The data of the example 
-- graph to the right can be found in the file p84.dat. 



-- A labeled graph in adjacency-list form
data LabeledAdjL a = LAL [(a,[(a,Int)])]
  deriving (Eq, Show)



-- Labeled edge-clause form
data LabeledEC a = LEC [LabeledEdge a]
  deriving (Eq, Show)
data LabeledEdge a = LEdge (a,a,Int) | LSingle a
  deriving (Eq, Show)


-- human-form for labeled undirected graphs example:
-- [a-b/1, a-c/1, c-b/2]
-- Convert labeled human-form to labeled edge-clause
lhf2lec::String->LabeledEC Char
lhf2lec st = LEC $ hfAux (drop 1 st) []
  where
  hfAux [] es             = es
  hfAux (c1:'-':c2:'/':cs) es =
    let (cost,cs') = break (\c -> '0' > c || c > '9') cs in
    hfAux (drop 2 cs') $ (LEdge (c1,c2, (read cost)::Int)):es
  hfAux (c:cs) es         = hfAux (drop 2 cs) $ (LSingle c):es

-- test
graph707 = lhf2lec "[a-b/1, a-c/1, c-b/2]"



-- Convert labeled edge-clause to labeled adjacency-list
lec2lal::(Eq a)=>LabeledEC a->LabeledAdjL a
lec2lal (LEC lec) = LAL $ adjList nodeList
  where
  nodeList = foldl nodeList2 [] lec
  nodeList2 ns (LEdge (x,y,cost)) = setCons y (setCons x ns)
  nodeList2 ns (LSingle x)        = setCons x ns

  adjList ns = map (\n -> foldl adjList2 (n,[]) lec) ns
  adjList2 n (LSingle _) = n
  adjList2 (n,adj) (LEdge (n1,n2,cost))
    |n == n1 = (n, setCons (n2,cost) adj)
    |n == n2 = (n, setCons (n1,cost) adj)
    |otherwise = (n, adj)

-- test
graph708 = lec2lal graph707


lhf2lal = lec2lal.lhf2lec


type PrimNode a = (
                   (a,[(a,Int)]), --n (node, adjacency-list)
                   Int,           --cost
                   Maybe (a,a)    --edge
                   )
-- (n, cost, edge)
-- The PrimNode is a shorthand for a triple used in the prim algorithm.
-- A PrimNode holds 3 pieces of information:
-- n is the node from a labeled adjacency-list
-- cost is the lowest cost found until now from any node n2 to n
-- edge is the edge from said node n2 to n

-- Retrieves the cost of a PrimNode
pnCost::PrimNode a->Int
pnCost (_, cost, _) = cost



-- ================================================================
-- SOLUTION
-- ================================================================
-- Given the proposed algorithm, it's simply a matter of implementation.
-- This implementation works only for connected graphs. A simple modification
-- where the result would be a list of lists of edges would suffice for the
-- algorithm to work on unconnected graphs as well (it would return a forest
-- instead of a tree).

-- The "queue" variable referenced in the body of the algortihm isn't really
-- a queue or a priority queue. It referenced in that way only because the
-- descriptions of the algorithm usually use the Q letter for the node set too.
-- Also elements of the queue are taken for computations based on the cost
-- attributed to them.

-- Steps:
-- 1. The "queue" is initialized including every node of the graph in a PrimNode
--    structure. All nodes start with cost equivalent to the maximum Int value
--    and no edge leading to them (Nothing, from the Maybe data type).
-- Repeat until there's no node on the queue:
  -- 2. A node n with the least recorded cost is taken from the queue. If it has
  --    an edge previously set as the lowest, this edge will be added to the list
  --    of edges that composes the minimum spanning tree.
  -- 3. The algorithm proceeds on updating the queue changing the cost of all
  --    PrimNodes that are in n's adjacency list. If the edge from n to some node n2
  --    and the PrimNode n2's recorded cost is higher than the cost from n1 to n2,
  --    this cost is changed for the cost from n1-n2 and PrimNode n2's edge is set
  --    to (n1-n2)

-- lowestCost and updateQ are functions used on prim, separated to make the
-- algorithm cleaner and more readable.

prim::(Eq a)=>LabeledAdjL a->[(a,a)]
prim (LAL lal) = primAux initQueue []
  where
  initQueue = [(n,maxBound::Int,Nothing) | n <- lal]
  primAux [] t = t
  primAux  q t = case lowestCost q of
                 ( pn@(n,cost,Nothing), q2) -> primAux (updateQ q2 pn) t
                 ( pn@(n,cost, Just e), q2) -> primAux (updateQ q2 pn) (e:t)
    
-- Finds the node with the lowest cost, removes it from the list and returns
-- both the node and the list without the node
lowestCost::[PrimNode a]->(PrimNode a, [PrimNode a])
lowestCost (pn:pns) = lowAux pn pns
  where
  lowAux lowest [] = (lowest, [])
  lowAux lowest (pn':pns')
    |pnCost pn' < pnCost lowest =
      let ( l, newQ) = lowAux pn' pns' in (l, lowest:newQ)
    |otherwise =
      let ( l, newQ) = lowAux lowest pns' in (l, pn':newQ)

-- For each node n1 in the adjacency-list of a given node n2, the function finds n
-- in the "queue" of primNodes. If the cost from n2 to n1 is lower then the
-- previous lowest recorded cost to said node n1, this new cost takes it's place
-- and the edge from n2 to n1 takes the place of the previous edge the PrimNode
-- had.
updateQ::(Eq a)=>[PrimNode a]->PrimNode a->[PrimNode a]
updateQ queue ((node,adjList),c,edges) = foldl updtAux queue adjList
  where
  -- updtAux::[PrimNode a]->(a,Int)->[PrimNode a]
  updtAux [] _ = []
  updtAux ( ((n,nes),cost1, e) :q) (adjn,cost2)
    |n == adjn && cost1 > cost2 = ((n,nes), cost2, Just (node,n) ) : q
    |otherwise                  = ((n,nes), cost1,             e ) : (updtAux q (adjn,cost2))

-- ================================================================

-- labeled undirected graphs for test

graph706 = LAL [('a', [('b',1),('c',1)]),
                ('b', [('a',1),('c',2)]),
                ('c', [('a',1),('b',2)]) ]

graph709 = lhf2lal "[a-b/5, a-d/3, b-c/2, b-e/4, c-e/6, d-e/7, d-f/4, d-g/3, e-h/5, f-g/4, g-h/1]"





-- 85

-- (**) Graph isomorphism

-- Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection f: 
-- N1 -> N2 such that for any nodes X,Y of N1, X and Y are adjacent if and only 
-- if f(X) and f(Y) are adjacent.

-- Write a predicate that determines whether two graphs are isomorphic. Hint: 
-- Use an open-ended list to represent the function f. 


eqSet::(Eq a)=>[a]->[a]->Bool
eqSet xs ys =
  (length xs == length ys) &&
  (and $ map (\x -> elem x ys) xs)
--

{-
-- An algorithm to make permutations of a list.
-- Found out later that there is a much faster one already on Data.List
permutations::[a]->[[a]]
permutations [] = [[]]
permutations xs = concat (map permutAux (takeOnAll xs))
  where
  permutAux (y,ys) = [ y:zs | zs <- permutations ys]

takeOnAll::[a]->[(a,[a])]
takeOnAll [] = []
takeOnAll (x:xs) =
  (x,xs):[ (y,x:ys) | (y,ys) <- takeOnAll xs]
-}



-- The applied solution involves making maps from the nodes of g1 to all 
-- possible permutations of nodes of g2.
-- 1. If the length of the list of nodes of g1 and g2 are not equal, 
--    evaluation returns False. Otherwise it proceeds.
-- 2. The algorithm will attempt to find the isomorphism for every 
--    possible bijection of both graphs. Of course, those are reffered to by 
--    the "bijections" function. This function actually constructs a Map type 
--    from Data.Map.Lazy where the keys are the nodes from g1 and the values 
--    are the nodes from a permutation of g2. What bijections returns is 
--    actually the lookup function for the constructed map.
-- 3. The algorithm for checking the isomorphism for a given bijection 
--    will, for every node in g1, map all it's adjacency-list with the 
--    bijection and then check if it's mirror node's adjacency-list in g2 is 
--    the same as the mapped list.
-- 4. If any of the bijections yelds True for 3, the function returns true.
iso::(Ord a, Eq b)=>AdjacencyList a->AdjacencyList b-> Bool
iso g1@(AdjL al1) g2@(AdjL al2) =
  (length al1 == length al2) && (or $ map isoAux bijections)
  where
  -- The list of all bijective functions from g1 to g2
  bijections =
    map (\p -> mapping al1 p Map.empty) (List.permutations al2)
    where
    mapping [] [] m                   = \k -> Maybe.fromJust (Map.lookup k m)
    mapping ((n1,_):xs) ((n2,_):ys) m = mapping xs ys (Map.insert n1 n2 m)
  -- Function that verifies if g1 is isomorphic to g2 for a given bijection b
  isoAux b = and $ map isoAux2 al1
    where
    isoAux2 (n1,nes1) =
      let (_, nes2) = getNode (b n1) g2 in
      eqSet (map b nes1) nes2

gt2 = GTerm ([1,2,3,4,5,6,7,8],[(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)])
gt1 = GTerm ([1,2,3,4,5,6,7,8],[(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)])
graph710 = gt2al gt1
graph711 = gt2al gt2

-- graph705 = hf2ec "[a-b, b-c, c-a]"
graph712 = ec2al graph705
graph713 = hf2al "[d-e, e-f, f-d]"

graph714 = hf2al "[1-2, 2-3, 2-4, 3-4, 2-5, 1-5]"
graph715 = hf2al "[2-1, 1-4, 1-3, 3-4, 2-5, 1-6]"



-- 86

-- (**) Node degree and graph coloration

-- a) Write a predicate degree(Graph,Node,Deg) that determines the degree of a 
-- given node.

-- b) Write a predicate that generates a list of all nodes of a graph sorted 
-- according to decreasing degree.

-- c) Use Welch-Powell's algorithm to paint the nodes of a graph in such a way 
-- that adjacent nodes have different colors. 

-- First 2 problems
nDegree::(Eq a)=>AdjacencyList a->a->Int
nDegree al n = length $ snd $ getNode n al

-- Note the comparisson is inverted because the exercise asks for a
-- decreasing sort.
sortByDegree::AdjacencyList a->[(a,[a])]
sortByDegree (AdjL al) = List.sortBy degreeCompare al
  where
  degreeCompare (n1,nes1) (n2,nes2) = compare (length nes2) (length nes1)

-- The algorithm for kcolor folows a pseudo code description of the 
-- suggested Welch-Powell's algorithm.
-- 1. Function colorize receives the list of nodes sorted by degree in 
--    decreasing order, an empty list that will store the colorized nodes -- as 
--    (node,color) tuples -- and then be returned and the integer 1 as the 
--    color tag which will be incremented on each recursion.
-- 2. For each node n' in the adjacency list of a node n, the function will 
--    search the list of already colorized nodes to check if n' is already 
--    colorized with the current color of the recursion. If not, then n will be 
--    colored with said color and stored in the list of colored nodes. Else the 
--    node will remain in the colorless original list and the next node will 
--    pass through the same check for the same color. After all nodes have been 
--    computed in this way, the recursion will repeat the process with a new 
--    color.
-- 3. The process ends when there are no more colorless nodes.
kcolor::(Eq a)=>AdjacencyList a->[(a,Int)]
kcolor adjl = colorize (sortByDegree adjl) [] 1
  where
  -- colorize::[(a,[a])]->[(a,Int)]->Int->[(a,Int)]
  colorize []    coloredNs     _ = coloredNs
  colorize graph coloredNs color = colorize newGraph newColoredNs (color+1)
    where
    (newGraph, newColoredNs) = colorizeAux graph coloredNs
    colorizeAux           [] cns = ([],cns)
    colorizeAux ((n,nes):ns) cns
      |or $ map isAdjacentToColor nes = ((n,nes):newG, newCNs)
      |otherwise                      = colorizeAux ns ((n,color):cns)
      where
      (newG,newCNs) = colorizeAux ns cns
      isAdjacentToColor n2 =
        case List.find (\(n3,c) -> n3 == n2 && c == color) cns of
        Nothing -> False
        Just _  -> True

-- Same test graph from the problems page:
graph716 = gt2al $ GTerm (['a','b','c','d','e','f','g','h','i','j'], [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')])



-- 87

-- (**) Depth-first order graph traversal (alternative solution)

-- Write a predicate that generates a depth-first order graph traversal 
-- sequence. The starting point should be specified, and the output should 
-- be a list of nodes that are reachable from this starting point (in 
-- depth-first order). 

-- An exceedingly simple problem to appear at this point in the exercise list.
-- Not sure why it is labeled as two star difficulty.
depthfirst::(Eq a)=>a->AdjacencyList a->[a]
depthfirst first adjl = reverse $ depthAux (getNode first adjl) [first]
  where
  depthAux (n,[]) res = res
  depthAux (n,(ne:nes)) res
    |elem ne res = depthAux (n,nes) res
    |otherwise   = depthAux (n,nes) (depthAux (getNode ne adjl) (ne:res))



-- 88

-- (**) Connected components (alternative solution)

-- Write a predicate that splits a graph into its connected components. 

connectedcomponents::(Eq a)=>AdjacencyList a->[[a]]
connectedcomponents (AdjL ((node,adjNodes):alTail)) = ccAux alTail adjNodes [node]
  where
  ccAux [] [] res           = [res]
  ccAux ((n,nes):al) [] res = res:(ccAux al nes [n])
  ccAux al (an:ans) res     = case findNremove an al of
    (Nothing,_)           -> ccAux al ans res
    (Just (n,nes), newAl) -> ccAux newAl (List.union nes ans) (n:res)
  
findNremove::(Eq a)=>a->[(a,[a])]->(Maybe (a,[a]),[(a,[a])])
findNremove _ [] = (Nothing, [])
findNremove n1 ((n2,nes):alT)
  |n1 == n2  = (Just (n2,nes),alT)
  |otherwise = (resNode, (n2,nes):resAl)
  where
  (resNode,resAl) = findNremove n1 alT

graph718 = gt2al (GTerm ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]))



-- 89

-- (**) Bipartite graphs

-- Write a predicate that finds out whether a given graph is bipartite. 

-- Because a bipartite graph only has edges between nodes of two different
-- sets, colorizing of a bipartite graph will result in exactly 2
-- different colors being used. That gives us a very simple predicate
-- using the above algorithm for kcolor. All that needs to be checked
-- is if the head* of the result of the kcolor function has color 2.
-- Any other color means there's only a single node on the graph (==1),
-- or the graph has more than 2 colors.
-- A more efficient algorithm could be a rewriten version of kcolor where
-- the current color is checked during the recursion and returns false
-- as soon as it goes above 2.

-- *The reason being this version of kcolor actually returns the colored nodes in
-- reverse color order: The first node in the list will have the last color
-- used in the colloring process.

bipartite::(Eq a)=>AdjacencyList a->Bool
bipartite adjl =
  let (_,color) = head $ kcolor adjl
  in color == 2

-- tests
graph719 = gt2al (GTerm ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)]))
graph720 = gt2al (GTerm ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)]))




