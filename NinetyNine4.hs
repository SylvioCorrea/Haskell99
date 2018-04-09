module NinetyNine4 where
import Test.QuickCheck


data Tree a = Empty | Branch {root::a,
                              leftT::Tree a,
                              rightT::Tree a}
              deriving (Eq)
--
instance (Show a)=> Show (Tree a) where
  show Empty                  = "Empty"
  show (Branch x Empty Empty) = "Leaf " ++ (show x)
  show (Branch x left right)  =
    "Brc " ++ (show x)
    ++ " (left: " ++ (show left) ++ ")"
    ++ " (right: " ++ (show right) ++ ")"

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Branch e lt rt) = Branch (f e) (fmap f lt) (fmap f rt)

leaf x = Branch x Empty Empty

nodeCount::Tree a->Int
nodeCount Empty = 0
nodeCount (Branch e treeL treeR) = 1 + (nodeCount treeL) + (nodeCount treeR)

treeHeight::Tree a->Int
treeHeight Empty = 0
treeHeight (Branch x left right) = 1 + (max (treeHeight left) (treeHeight right))

isHbalanced::Tree a->Bool
isHbalanced Empty                   = True
isHbalanced (Branch x leftT rightT) =
  isHbalanced leftT &&
  isHbalanced rightT &&
  -1 <= heightDif && 
  heightDif <= 1
  where
  heightDif = treeHeight leftT - treeHeight rightT



-- 54
-- Check whether a given term represents a binary tree
-- That is not a problem in Haskell :(

-- 55
-- Create a balanced tree with n nodes.
-- The tree is considered balanced if
-- the difference in the number of nodes in it's
-- left and right subtrees do not exceed 1.
cbalTree::Int->[Tree Char]
cbalTree 0 = [Empty]
cbalTree n 
  |half1 == half2 = [ Branch 'x' tree tree | tree <- cbalTree half1 ]
  |otherwise      =
    [ Branch 'x' tree1 tree2 | tree1 <- cbalTree half1, tree2 <- cbalTree half2 ]
    ++
    [ Branch 'x' tree2 tree1 | tree1 <- cbalTree half1, tree2 <- cbalTree half2 ]
  where
    half1 = (n-1) `div` 2
    half2 = (n-1) - half1

-- 56
-- Symmetric binary trees: the structure of the tree is mirrored horizontally
symetric::(Eq a)=>Tree a -> Bool
symetric Empty = True
symetric (Branch t left right) = left `mirror` right

mirror::(Eq a)=>Tree a->Tree a->Bool
mirror Empty Empty = True
mirror Empty _     = False
mirror _     Empty = False
mirror (Branch t1 left1 right1) (Branch t2 left2 right2) =
  left1 `mirror` right2 && right1 `mirror` left2

-- Test trees
tree1 = Branch 'x' (Branch 'x' (leaf 'x') Empty)
                   (Branch 'x' Empty (leaf 'x'))

tree2 = Branch 'x' (Branch 'x' (leaf 'x') Empty)
                   (Branch 'x' (leaf 'x') Empty)

-- 57
-- Binary Search Tree Construction

construct::(Ord a)=>[a]->Tree a
construct [] = Empty
construct xs = construct2 xs Empty where
  construct2 [] tree     = tree
  construct2 (e:es) tree = construct2 es (treeInsert e tree)

treeInsert::(Ord a)=> a -> Tree a -> Tree a
treeInsert e Empty = leaf e
treeInsert e (Branch x l r)
  |e <= x    = Branch x (treeInsert e l) r
  |otherwise = Branch x l (treeInsert e r)

test401 = (symetric.construct) [5,3,18,1,4,12,21]
test402 = (symetric.construct) [3,2,5,7,1]

-- 58
-- Construct symetrical balanced trees of n nodes using the
-- generate-and-test paradigm.

-- This solution generates symetric 
-- trees using a precise algorithm, not the sugested paradigm. One 
-- solution using the sugested paradigm could be made by simply 
-- applying filter with the symetric property, as it is shown in the 
-- solutions page
symCbalTrees::Int->[Tree Char]
symCbalTrees 0 = [Empty]
symCbalTrees n
  |n `mod` 2 == 0 = []
  |otherwise =
    [Branch 'x' tree1 tree2 | (tree1,tree2) <- zippedList]
  where
    zippedList = zip treeList (reverse treeList)
    treeList   = cbalTree $ n `div` 2

{-
prop_symCbalTrees::Int->Bool
prop_symCbalTrees n =
  n `mod` 2 == 0 || (and $ map symetric $ symCbalTrees n)
-}

test403 n = (and $ map symetric $ symCbalTrees n)

-- 59
-- Construct all possible height-balanced binary trees with a
-- given height and element. A tree is considered height-
-- balanced when the height of it's right and left subtrees have a
-- difference of at most 1.
hbalTree::a->Int->[Tree a]
hbalTree e 0 = [Empty]
hbalTree e 1 = [leaf e]
hbalTree e n =
  [ Branch e tree1 tree2 | tree1 <- subtrees1, tree2 <- subtrees1]
  ++
  [ Branch e tree1 tree2 | tree1 <- subtrees2, tree2 <- subtrees1]
  ++
  [ Branch e tree1 tree2 | tree1 <- subtrees1, tree2 <- subtrees2]
  where
  subtrees1 = hbalTree e $ n-1
  subtrees2 = hbalTree e $ n-2
  

heightCheck::Int->Bool
heightCheck n =
  and $ map (==n) $ map treeHeight $ hbalTree 'x' n








-- 60

-- Construct height-balanced binary trees with a given number of nodes

-- Consider a height-balanced binary tree of height H. What is the 
-- maximum number of nodes it can contain?
-- Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? 
-- This question is more difficult. Try to find a recursive statement 
-- and turn it into a function minNodes
-- that returns the minimum number of nodes in a height-balanced binary 
-- tree of height H. On the other hand, we might ask: what is the 
-- maximum height H a height-balanced binary tree with N nodes can have? 
-- Write a function maxHeight
-- that computes this.

-- Now, we can attack the main problem: construct all the 
-- height-balanced binary trees with a given number of nodes. Find out 
-- how many height-balanced trees exist for N = 15. 

maxNodes::Int->Int
maxNodes h = 2^h - 1

-- The minimum number of nodes in this definition of a height balanced tree,
-- given a height h, is obtained by the sum of the mimimum number of nodes 
-- of a tree of height h-1 (one of the subtrees of the root) and another
-- subtree of height h-2 (the other subtree of the root), plus the height
-- of the root (1).
minNodes::Int->Int
minNodes 0 = 0
minNodes 1 = 1
minNodes h = minNodes (h-1) + minNodes (h-2) + 1

-- While attempting to find a pattern to solve maxHeight it became apparent
-- that the differences between the minimum nodes of h+1 height trees
-- and an h height trees formed the fibonacci sequence.

-- The following asumes the fibonacci sequence starting in index 0 = 1

-- Returns a reverse fibonacci sequence
fibList::Int->[Int]
fibList 0 = [1]
fibList 1 = [1,1]
fibList n = (sum $ take 2 fibList') : fibList'
  where
  fibList' = fibList $ n-1

-- Returns a list of node differences between consecutive minimal balanced
-- trees
diffNodes::Int->[Int]
diffNodes 0 = [0]
diffNodes h = diffNodes2 nodes
  where
  diffNodes2 [n1,n2] = [n2-n1]
  diffNodes2 (n1:n2:ns) = (n2-n1) : (diffNodes2 $ n2:ns)
  nodes = map minNodes [0..h]

-- Not only that: the (i+1)th fibonacci number corresponded to the minimum
-- number of nodes - 1 of a balanced tree of height i
propFibTree::Int->Bool
propFibTree n = (fib (n+1) - 1) == (minNodes n)

propTest n = and $ map propFibTree [0..n]

-- Therefore, the recursion can be rewriten into a faster fibonacci
-- calculation:
minNodesF::Int->Int
minNodesF h = fib (h+1) - 1

fib::Int->Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- That insight can be used to construct the maxHeight function
maxHeight::Int->Int
maxHeight 0 = 0
maxHeight 1 = 1
maxHeight n = maxHeight2 [2,1]
  where
  maxHeight2 xs
    |soma2 - 1 > n = length xs - 1
    |otherwise = maxHeight2 $ soma2:xs
    where
    soma2 = sum $ take 2 xs

minHeight::Int->Int
minHeight n = minHeight2 n 0
  where
  minHeight2 n p
    |2^p - 1 >= n = p
    |otherwise    = minHeight2 n (p+1)

-- This test demonstrates that the minmum number of nodes in a balanced tree
-- tree of height h, when used in the function maxHeight, returns exactly h
testMaxHeight h =
  and $ map (\n -> maxHeight (minNodesF n) == n) [0..h]
  


  
  
  
-- ==========================================================================
-- ========================SOLUTION 1========================================
-- ==========================================================================

-- Finally, this function constructs all possible balanced binary trees with
-- a given number of nodes. It actually generates all height balanced trees
-- with heights between the maximum and minimum heights of a tree with n nodes,
-- (which ends up genrating trees that don't have n nodes) then it filters
-- the results to return only the trees with n nodes.
hbalTreeNodes::a->Int->[[Tree a]]
hbalTreeNodes e n =
  map (filter (\t -> nodeCount t == n)) $ map (hbalTree e) [(minHeight n)..(maxHeight n)]

-- ==========================================================================
-- ==========================================================================





-- This function is used to count all elements inside a list of lists.
nestedCounter::[[a]]->Int
nestedCounter xs = sum $ map length xs

-- Counts the number of generated trees by hbalTreeNodes
counthbtn::Int->Int
counthbtn n = nestedCounter $ hbalTreeNodes 'a' n






-- ==========================================================================
-- ========================SOLUTION 2========================================
-- ==========================================================================
-- This solution is incorrect comparing it with the generating solution
-- given in the solutions page. It will remain here for future correction.

-- This function generates ONLY the height balanced trees with n nodes.
-- This is correct only up to n = 19.
hbalTreeNodesE::a->Int->[Tree a]
hbalTreeNodesE e n = hbtnE n (maxHeight n)
  where
  hbtnE 0 _ = [Empty]
  hbtnE 1 _ = [leaf e]
  hbtnE n' h = concat $ map auxF (nodeTuples (n'-1) h)
  -- One of the nodes n' will be used as the root of this subtree.
  -- The distribution of the remaining nodes will be given by the nodeTuples function
    where
    auxF (t1@(n1,h1),t2@(n2,h2)) =
      let tree1 = [Branch e leftT rightT | leftT <- hbtnE n1 h1, rightT <- hbtnE n2 h2]
          tree2 = [Branch e leftT rightT | leftT <- hbtnE n2 h2, rightT <- hbtnE n1 h1]
      in if t1==t2 then tree1 else tree1 ++ tree2

-- This function finds all possible node partitions between the two subtrees
-- of a balanced tree of height h and n+1 nodes.
nodeTuples::Int->Int->[((Int,Int),(Int,Int))]
nodeTuples n h = nodeTuples2 (n `div` 2) (n - (n `div` 2))
  where
  nodeTuples2 m1 m2
    |minN <= m1 && m2 <= maxN = (tGen m1, tGen m2) : (nodeTuples2 (m1-1) (m2+1))
    |otherwise                = []
  -- Any balanced tree of height h will have a root with subtrees of the same height h-1 or
  -- one subtree of height h-1 and another of height h-2
  minN = minNodes (h-2)
  maxN = maxNodes (h-1)
  maxHminN = minNodes (h-1) -- The minimum number of nodes in a tree of height h-1
  tGen n'
    |n' >= maxHminN = (n', h-1)
    |otherwise      = (n', h-2)
    -- It is possible for the same number of nodes to be able to generate height balanced
    -- trees of different heights. The way the tree generating function is built, any 
    -- balanced trees of a given height h or below will be constructed, therefore, it
    -- only matters that the funtion returns the highest possible height for any given
    -- number of nodes.
    -- It's important that the height is passed along with the recursion because there
    -- are times in which the possible heights of subtrees with a given number of nodes
    -- are not in accordance with the height limit imposed on that subtree by the previous
    -- computations.

-- Counts the number of generated trees by hbalTreeNodesE
counthbtnE n = length $ hbalTreeNodesE 'a' n

-- Compares the number of generated trees by hbalTreeNodes and hbalTreeNodesE
compareCount n = (counthbtn n, counthbtnE n)





-- Generating algorithm given by the solution's page
hbalTreeNodes' :: a -> Int -> [Tree a]
hbalTreeNodes' x n = [t | h <- [minHeight n .. maxHeight n], t <- baltree h n]
  where
        -- baltree h n = weight-balanced trees of height h with n nodes
        -- assuming minNodes h <= n <= maxNodes h
        baltree 0 n = [Empty]
        baltree 1 n = [Branch x Empty Empty]
        baltree h n = [Branch x l r |
                (hl,hr) <- [(h-2,h-1), (h-1,h-1), (h-1,h-2)],
                let min_nl = max (minNodes hl) (n - 1 - maxNodes hr),
                let max_nl = min (maxNodes hl) (n - 1 - minNodes hr),
                nl <- [min_nl .. max_nl],
                let nr = n - 1 - nl,
                l <- baltree hl nl,
                r <- baltree hr nr]









-- Previous failures:

{-
nodeTuples'::Int->Int->[(Int,Int)]
nodeTuples' n h = nodeTuples2 (n `div` 2) (n - (n `div` 2))
  where
  nodeTuples2 m1 m2
    |minN <= m1 && m2 <= maxN = (m1,m2) : (nodeTuples2 (m1-1) (m2+1))
    |otherwise                = []
  minN = minNodes (h-2)
  maxN = maxNodes (h-1)
  

nodeTuples::Int->Int->[((Int,Int),(Int,Int))]
nodeTuples n h = nodeTuples2 (n `div` 2) (n - (n `div` 2))
  where
  nodeTuples2 m1 m2
    |minN <= m1 && m2 <= maxN = (tGenerate m1 m2) : (nodeTuples2 (m1-1) (m2+1))
    |otherwise                = []
  minN = minNodes (h-2)
  maxN = maxNodes (h-1)
  minHthreshold = maxNodes (h-2)
  maxHthreshold = minNodes (h-1)
  targetHeight n'
    |n' > threshold = h-1
    |otherwise      = h-2
  tGenerate n1 n2
    |n1
    |(n', targetHeight n') -- Generates tuples consisting of the number of nodes required for one subtree along with it's target height
-}
{-
-- This function finds all possible node partitions between the two subtrees of a balanced tree of height h and n+1 nodes.
nodeTuples::Int->Int->[((Int,Int),(Int,Int))]
nodeTuples n h = nodeTuples2 (n `div` 2) (n - (n `div` 2))
  where
  nodeTuples2 m1 m2
    |minN <= m1 && m2 <= maxN = [(t1,t2) | t1 <- tGen m1, t2 <- tGen m2] ++ (nodeTuples2 (m1-1) (m2+1))
    |otherwise                = []
  -- Any balanced tree of height h will have a root with subtrees of the same height h-1 or
  -- one subtree of height h-1 and another of height h-2
  minN = minNodes (h-2)
  maxN = maxNodes (h-1)
  minHmaxN = maxNodes (h-2) -- The maximum number of nodes in a tree of height h-2
  maxHminN = minNodes (h-1) -- The minimum number of nodes in a tree of height h-1
  tGen n' = if n' <= minHmaxN  -- There exists a tree of height n-2 with n' nodes
            then if maxHminN <= n' -- There also exists a tree with height h-1 with n' nodes
                 then [(n',h-2),(n',h-1)]
                 else [(n',h-2)]
            else [(n',h-1)]
            -- Any number of nodes n' passed to this function is guaranteed to be
            -- inside these boundaries because of the test performed in nodeTuples2's first guard
-}
