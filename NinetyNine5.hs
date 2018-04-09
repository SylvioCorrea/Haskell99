module NinetyNine5 where
import NinetyNine4

-- 61
-- Count the leaves of a binary tree
-- A leaf is a node with no successors. Write a predicate count_leaves/2
-- to count them.

isLeaf::Tree a->Bool
isLeaf(Branch e Empty Empty) = True
isLeaf _                        = False

isEmptyT::Tree a->Bool
isEmptyT Empty = True
isEmptyT _     = False

leafOrEmpty t = (isLeaf t || isEmptyT t)

countLeaves::Tree a->Int
countLeaves Empty = 0
countLeaves (Branch e Empty Empty) = 1
countLeaves (Branch e leftT rightT) = (countLeaves leftT) + (countLeaves rightT)

-- test trees
tree501 = Branch 1 (leaf 2) tree502
tree502 = Branch 3 (leaf 4) Empty
tree503 = Branch 1 tree504 tree502
tree504 = Branch 2 Empty (leaf 5)

tree505 = Branch 2 (leaf 6) (leaf 5)
tree506 = Branch 7 (leaf 8) Empty
tree507 = Branch 4 tree506 Empty
tree508 = Branch 3 tree507 Empty
tree509 = Branch 1 tree505 tree508



--61-A
-- Collect the leaves of a binary tree in a list
-- A leaf is a node with no successors. Write a predicate
-- leaves/2 to collect them in a list.

leaves::Tree a->[a]
leaves Empty = []
leaves t
  |isLeaf t  = [root t]
  |otherwise = (leaves $ leftT t) ++ (leaves $ rightT t)


-- 62
-- Collect the internal nodes of a binary tree in a list
-- An internal node of a binary tree has either one or two non-empty
-- successors. Write a predicate internals/2 to collect them in a list.

internals::Tree a->[a]
internals t
  |not (leafOrEmpty t) = (root t) : ( (internals $ leftT t)++(internals $ rightT t) )
  |otherwise           = []


-- 63
-- Construct a complete binary tree
-- A complete binary tree with height H is defined as follows:
    
    -- The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 
    -- 2**(i-1) at the level i)
    
    -- In level H, which may contain less than the maximum possible number 
    -- of nodes, all the nodes are "left-adjusted". This means that in a 
    -- levelorder tree traversal all internal nodes come first, the leaves 
    -- come second, and empty successors (the nil's which are not really 
    -- nodes!) come last. 

-- Particularly, complete binary trees are used as data structures (or 
-- addressing schemes) for heaps.
-- We can assign an address number to each node in a complete binary tree by 
-- enumerating the nodes in level-order, starting at the root with number 1. 
-- For every node X with address A the following property holds: The address 
-- of X's left and right successors are 2*A and 2*A+1, respectively, if they 
-- exist. This fact can be used to elegantly construct a complete binary 
-- tree structure.
-- Write a predicate complete_binary_tree/2. 

completeBinaryTree::Int->Tree Char
completeBinaryTree n = cbt 1
  where
  cbt x
    |x>n = Empty
    |otherwise = Branch 'x' (cbt $ 2*x) (cbt $ 2*x + 1)


-- 64
-- Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a 
-- preparation for drawing the tree, a layout algorithm is required to 
-- determine the position of each node in a rectangular grid. Several 
-- layout methods are conceivable, one of them is shown in the illustration 
-- below:

  -- (visit https://wiki.haskell.org/99_questions/61_to_69 to see the
  -- illustration)

-- In this layout strategy, the position of a node v is obtained by the 
-- following two rules:

    -- x(v) is equal to the position of the node v in the inorder sequence
    -- y(v) is equal to the depth of the node v in the tree 

-- Write a function to annotate each node of the tree with a position, 
-- where (1,1) in the top left corner or the rectangle bounding the drawn 
-- tree. 

-- Must not be used on empty nodes.
getNodeX, getNodeY::Tree (a,(Int,Int))->Int
getNodeX = fst.snd.root
getNodeY = snd.snd.root

layout::Tree a->Tree (a,(Int,Int))
layout tree = layoutAux tree 1 1
  where
  layoutAux Empty _ _ = Empty
  layoutAux (Branch e lt rt) x y =
    case layoutAux lt x (y+1) of 
         Empty -> Branch (e,(x,y)) Empty (layoutAux rt (x+1) (y+1))
         lt'   -> let (_,(ltx,_)) = root lt'
                  in Branch (e,(ltx+1,y)) lt' (layoutAux rt (ltx+1) (y+1))
         

-- 65
-- An alternative layout method is depicted in the illustration below:

  -- (visit https://wiki.haskell.org/99_questions/61_to_69 to see the
  -- illustration)

-- Find out the rules and write the corresponding function. Hint: On 
-- a given level, the horizontal distance between neighboring nodes 
-- is constant.
-- Use the same conventions as in problem P64 and test your function 
-- in an appropriate way. 

layout2::Tree a->Tree (a,(Int,Int))
layout2 tree = layoutAux tree 0 1
  where
  h = treeHeight tree
  
  newX x' y'
    |x' == 0   = 1
    |otherwise = x' + 2^(h-y')
  
  layoutAux Empty _ _ = Empty
  layoutAux (Branch e lt rt) x y =
    case layoutAux lt x (y+1) of
         Empty -> Branch (e, (x1, y)) Empty (layoutAux rt x1 $ y+1)
           where x1 = newX x y
         lt'   -> Branch (e, (x2, y)) lt' (layoutAux rt x2 $ y+1)
           where x2 = newX (getNodeX lt') (y+1)


-- 66
-- Yet another layout strategy is shown in the illustration 
-- below:

  -- (visit https://wiki.haskell.org/99_questions/61_to_69 to see the
  -- illustration)

-- The method yields a very compact layout while maintaining a 
-- certain symmetry in every node. Find out the rules and write 
-- the corresponding Prolog predicate. Hint: Consider the 
-- horizontal distance between a node and its successor nodes. 
-- How tight can you pack together two subtrees to construct 
-- the combined binary tree?
-- Use the same conventions as in problem P64 and P65 and test 
-- your predicate in an appropriate way. Note: This is a 
-- difficult problem. Don't give up too early!



-- The following functions and types before the solution will be used
-- to decorate the tree with the left and right width of each of it's
-- subtrees. This data will be used to attack the main problem.

data TWidths = TW Int Int
               deriving (Eq, Show)

getLW, getRW::Tree (a, TWidths)->Int
getLW Empty = error "Empty trees have no width"
getLW (Branch (_, TW lw _) _ _) = lw
getRW Empty = error "Empty trees have no width"
getRW (Branch (_, TW _ rw) _ _) = rw

-- Synonym for a binary tree decorated with it's right and left widths:
type DecorTW a = Tree (a,TWidths)

-- Decorates the tree nodes with the width of their left and right
-- subtrees
widthDecor::Tree a -> DecorTW a
widthDecor Empty = Empty
widthDecor (Branch e Empty Empty) = Branch (e, TW 0 0) Empty Empty
widthDecor (Branch e leftT rightT) = Branch (e, TW lw rw) lt rt
  where
  (lw,rw) = lrWidth lt rt d
  d  = hDistance lt rt
  lt = widthDecor leftT
  rt = widthDecor rightT

undecorate::DecorTW a->Tree a
undecorate = fmap fst

-- Function that determines what is the horizontal distance between
-- one node and it's two children. In layout3, this distance is equal
-- for left and right.
hDistance::DecorTW a->DecorTW a->Int
hDistance Empty rt = 1
hDistance lt Empty = 1
hDistance lt rt
  |rwlt > lwrt = rwlt + 1 - (rwlt - lwrt)
  |otherwise   = lwrt + 1 - (lwrt - rwlt)
  where
  rwlt = getRW lt
  lwrt = getLW rt

-- Function that determines what are the widths of some tree to the
-- left and to the right of it's root. Receives left and right
-- decorated subtrees together with the hDistance of the root
lrWidth::DecorTW a->DecorTW a->Int->(Int,Int)
lrWidth lt rt d = (lw,rw)
  where
  -- lw: width of the tree to the left
  lw 
    |isEmptyT lt = max 0 (getLW rt - 1)
    |isEmptyT rt = max 0 (getLW lt + 1)
    |otherwise   = max (getLW lt + d) (getLW rt - d)
  -- rw: width of the tree to the right
  rw 
    |isEmptyT lt = max 0 (getRW rt + 1)
    |isEmptyT rt = max 0 (getRW lt - 1)
    |otherwise   = max (getRW lt - d) (getRW rt + d)

-- ================================================================
--                    SOLUTION
-- ================================================================
-- The strategy:
-- 1. Decorate the tree and it's subtrees with the widths of it's left
--    and right subtrees. This will avoid the need to recalculate these
--    values at every point in the recursion.
-- 2. Starting layout3A, find the left most node by checking, with the
--    help of left width data
--    from subtrees, which subtree expands more to the left.
-- 3. Once the leftmost node is found, it's x coordinate (1) is used by
--    previous nodes of the recursion to find their own x coordinates
--    by calculating the horizontal distance from themselves to the child
--    with the known x value.
-- 4. Some subtrees won't have been visited by this recursion because their
--    siblings expanded more to the left. Those trees will receive the x
--    coordinate from their parents already corrected for horizontal distance
--    and then will start a new recursion down their respective subtrees
--    with function layout3B.
-- 5. The y coordinate will be given by a height argument passed along
--    in both functions.
-- 6. Undecorate removes the width data from the nodes, leaving only the
--    original node data and the coordinates.
layout3::Tree a->Tree (a,(Int,Int))
layout3 Empty = Empty
layout3 t     = undecorate $ layout3A (widthDecor t) 1

-- Arguments: decorated tree and height
layout3A::DecorTW a->Int->DecorTW (a,(Int,Int))
layout3A (Branch (e,tw) lt rt) h
  |isEmptyT lt && isEmptyT rt = Branch ((e,(1,h)), tw) Empty Empty
  |isEmptyT rt                = Branch ((e, (getX newlt + 1,h) ),tw) newlt Empty
  |isEmptyT lt                = if getLW rt > 2
                                then layout3A rt (h+1)
                                else Branch ((e,(1,h)), tw) Empty (layout3B rt (h+1) 2)
  |getLW lt +d > getLW rt -d  = let x = getX newlt + d in
    Branch ((e,(x,h)),tw) newlt (layout3B rt (h+1) (x + d))
  |otherwise                  = let x = getX newrt - d in
    Branch ((e,(x,h)),tw) (layout3B lt (h+1) (x - d)) newrt
  where
  newlt = layout3A lt (h+1)
  newrt = layout3A rt (h+1)
  d     = hDistance lt rt
  getX  = fst.snd.fst.root -- returns the x coordinate from the tuple ((e,(x,y)), tw)





-- Arguments: decorated tree, height of the node, x value of the node on
-- the grid
layout3B::DecorTW a->Int->Int->DecorTW (a,(Int,Int))
layout3B Empty h x = Empty
layout3B (Branch (e,tw) lt rt) h x =
  Branch ((e,(x,h)),tw) (layout3B lt (h+1) (x-d)) (layout3B rt (h+1) (x+d))
  where
  d = hDistance lt rt



-- 67-A
-- A string representation of binary trees

-- Somebody represents binary trees as strings of the following type:

    -- a(b(d,e),c(,f(g,))) 

-- a) Write a Prolog predicate which generates this string representation, 
-- if the tree is given as usual (as nil or t(X,L,R) term). Then write a 
-- predicate which does this inverse; i.e. given the string 
-- representation, construct the tree in the usual form. Finally, combine 
-- the two predicates in a single predicate tree_string/2 which can be 
-- used in both directions. 


-- treeToString is much like implementing a Show instance. No real
-- difficulty here.
treeToString::(Show a)=>Tree a->String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = show x
treeToString (Branch x lt rt) =
  (show x) ++ "(" ++ (treeToString lt) ++ "," ++ (treeToString rt) ++ ")"
--

-- stringToTree should benefit from a proper parser, but it's simple enough
-- that one does not need to rely on any libraries.
stringToTree::String->Tree Char
stringToTree [] = Empty
stringToTree (c:cs) = Branch c (stringToTree cs1) (stringToTree cs2)
  where
  (cs1,cs2) = stringSubtree cs

-- This is the most interesting function of the three, used to split the strings
-- of left and right subtrees. It returns a tuple with both. bs is a counter
-- for the number of unclosed brackets that were encountered during the
-- recursion. A comma met outside of such unclosed brackets signifies the
-- ending of the description of the left tree and the beggining of the
-- description of the right tree. At this point, the recursion alters the way it
-- integrates the remaining chars with the result string tuple, changing from
-- cons1 (list construction with the first list of the tuple) to cons2 (list
-- construction with the second list of the tuple).
stringSubtree::String->(String, String)
stringSubtree [] = ("","")
stringSubtree ('(':cs) = aux cs 0 cons1
  where
  aux (')':xs) bs consx
    |bs==0     = ("","")
    |otherwise = consx ')' (aux xs  (bs-1) consx)
  aux (',':xs) bs consx
    |bs==0     = aux xs bs cons2
    |otherwise = consx ',' (aux xs bs consx)
  aux ('(':xs) bs consx = consx '(' (aux xs (bs + 1) consx)
  aux (x:xs) bs consx   = consx x (aux xs bs consx)
  cons1 = \x (st1,st2)-> (x:st1,st2)
  cons2 = \x (st1,st2)-> (st1,x:st2)

string501 = "a(b,c)"
string502 = "a(,c)"
string503 = "a(b,)"
string504 = "a(b(c,d),e(f,g(h,i)))"



-- 68
-- Preorder and inorder sequences of binary trees. We consider binary trees 
-- with nodes that are identified by single lower-case letters, as in the 
-- example of problem P67.

-- a) Write predicates preorder/2 and inorder/2 that construct the preorder 
-- and inorder sequence of a given binary tree, respectively. The results 
-- should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in 
-- problem P67.

-- b) Can you use preorder/2 from problem part a) in the reverse direction; 
-- i.e. given a preorder sequence, construct a corresponding tree? If not, 
-- make the necessary arrangements.

-- c) If both the preorder sequence and the inorder sequence of the nodes of a 
-- binary tree are given, then the tree is determined unambiguously. Write a 
-- predicate pre_in_tree/3 that does the job. 



-- About b), you cannot reconstruct a tree out of only one traversal
-- string representation of a tree. One could make a reconstruction algorithm,
-- but not guarantee that the reconstructed tree is equal to the traversed one.

treeToInorder::Tree Char->String
treeToInorder Empty = ""
treeToInorder (Branch e lt rt) = (treeToInorder lt) ++ [e] ++ (treeToInorder rt)

treeToPreorder::Tree Char->String
treeToPreorder Empty = ""
treeToPreorder (Branch e lt rt) = [e] ++ (treeToPreorder lt) ++ (treeToPreorder rt)

tree510 = stringToTree "a(b(d,e),c(,f(g,)))"

-- Receives preorder and inorder string representations of the same tree,
-- using them to rebuild the tree.
preInTree::String->String->Tree Char
preInTree _ "" = Empty
preInTree (p:ps) io = Branch p (preInTree ps1 lt) (preInTree ps2 rt)
  where
  (lt,rt)   = (\(l1,l2) -> (l1,tail l2)) $ span (/=p) io
  (ps1,ps2) = span (\x -> elem x lt) ps


testPreIn t = preInTree (treeToPreorder t) (treeToInorder t)



-- 69
-- Dotstring representation of binary trees.

-- We consider again binary trees with nodes that are identified by 
-- single lower-case letters, as in the example of problem P67. Such a 
-- tree can be represented by the preorder sequence of its nodes in which 
-- dots (.) are inserted where an empty subtree (nil) is encountered 
-- during the tree traversal. For example, the tree shown in problem P67 
-- is represented as 'abd..e..c.fg...'. First, try to establish a syntax 
-- (BNF or syntax diagrams) and then write a predicate tree_dotstring/2 
-- which does the conversion in both directions. Use difference lists. 


tree2ds::Tree Char->String
tree2ds Empty = "."
tree2ds (Branch c lt rt) = c:((tree2ds lt) ++ (tree2ds rt))

-- Using difference lists:
-- ======================================================================
-- These DList definitions where taken from
-- https://hackage.haskell.org/package/dlist
-- A DList package by Don Stewart and Sean Leather. This is just a small
-- part of the package.

newtype DList a = DL { unDL :: [a] -> [a] }

toList:: DList a -> [a]
toList = ($[]) . unDL

singleton:: a -> DList a
singleton = DL . (:)

infixr `cons`
cons:: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)

append:: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
-- ======================================================================

tree2dsDL::Tree Char->String
tree2dsDL tree = toList $ dsAux tree
  where
  dsAux Empty = singleton '.'
  dsAux (Branch c lt rt) = cons c (append (dsAux lt) (dsAux rt))



ds2tree::String->Tree Char
ds2tree "." = Empty
ds2tree (c:cs) = Branch c (ds2tree splitL) (ds2tree splitR)
  where
  (splitL, splitR) = dsSplit cs

-- This function splits the remaining dot string whose head was used to create a
-- tree branch in ds2tree. This means this remaining string contains the
-- description of the two subtrees of said branch. The strategy to find these
-- two subtrees is the following:
-- 1. If the string starts with a dot, it means the left subtree is empty and
--    the rest of the dot string represents the right subtree
-- 2. Otherwise, the first charactere is not a dot and we should find a
--    sequence of 2 dots in the remaining list which will give us the clue as to
--    where the string should be split. The reasoning behind this is that there
--    must be at least one node in this subtree with no children.
-- 3. However, this sequence of 2 dots isn't enough to guarantee we have found
--    the entire left tree. For this to be true, we need to have found one dot
--    more than we have found other characters, otherwise, there would be some
--    node in the subtree without the description of one of it's own subtrees.
--    Such is the use for the function dotDiff.
dsSplit::String->(String,String)
dsSplit ('.':cs) = (".",cs)
dsSplit st = dsAux st ""
  where
  dsAux ('.':xs) res
    |head xs == '.' && dotDiff newRes == 1 = (reverse newRes, tail xs)
    |otherwise = dsAux xs ('.':res)
    where
    newRes = ('.':'.':res)
  dsAux (x:xs) res = dsAux xs (x:res)
  
-- Function that returns the difference between the number of dots and
-- non dot characters in a string
dotDiff::String->Int
dotDiff = sum.map (\x -> if x == '.' then 1 else (-1))

ds501 = "abc...."
ds502 = "a.."
ds503 = "abc..de...f.."






