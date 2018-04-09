module NinetyNine6 where

-- Multiway trees
-- A multiway tree is composed of a root element and a (possibly empty) set 
-- of successors which are multiway trees themselves. A multiway tree is 
-- never empty. The set of successor trees is sometimes called a forest.


data Tree a = Node a [Tree a]
              deriving (Eq, Show)

-- Test trees
tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

tree6 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' [Node 'h' []]]
                ]
-- 70-B
-- (*) Check whether a given term represents a multiway tree.

-- In Prolog or Lisp, one writes a predicate to check this.

-- As in problem 54A, all members of this type are multiway trees; there is no 
-- use for a predicate to test them.


-- 70-C

-- (*) Count the nodes of a multiway tree.

nodeCount::Tree a->Int
nodeCount (Node a []) = 1
nodeCount (Node a fr) = 1 + (sum $ map nodeCount fr)


-- 70

-- (**) Tree construction from a node string.

-- We suppose that the nodes of a multiway tree contain single characters. 
-- In the depth-first order sequence of its nodes, a special character ^ 
-- has been inserted whenever, during the tree traversal, the move is a 
-- backtrack to the previous level.

-- By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^

  -- visit https://wiki.haskell.org/99_questions/70B_to_73
  -- to see the picture

-- Define the syntax of the string and write a predicate tree(String,Tree) 
-- to construct the Tree when the String is given. Make your predicate work 
-- in both directions. 

treeToString::Tree Char->String
treeToString t = (t2s t)
  where
  t2s (Node c fr) = c:(concat $ map t2s fr)++"^"

stringToTree::String->Tree Char
stringToTree (c:cs) = Node c $ map stringToTree $ splitString cs

-- splitString is the most interesting function here. It takes the tail
-- of a string representation of a tree and finds out how to separate it
-- into a list of strings, each representing one of it's subtrees.
-- The algorithm:
-- 1. If the string is a single '^', there are no subtrees.
-- 2. Each character that isn't a '^' means we're going down further levels
--    in a subtree. These levels are kept by the lvl counter. Each '^'
--    character means we're going back one level above in the description
--    of the subtree. When a '^' is found and we're a single level below
--    the root, it means we came back to the root and all previous 
--    characters were the description of a single one of it's subtrees. This 
--    description is kept and the recursion starts anew to find out any 
--    remaining subtrees.
-- 3. The characters of a subtree are collected by tst (tree string) during
--    the execution. tst is actually exactly like a difference list. The point
--    is to avoid appending on common lists to keep the order of the collected
--    characters -- (tst.(c:)) instead of the common list (tst ++ [c])

splitString::String->[String]
splitString "^" = []
splitString st  = spst st id 0
  where
  spst ('^':cs) tst lvl
    |lvl == 1  = (tst "^") : (splitString cs)
    |otherwise = spst cs (tst.('^':)) (lvl-1)
  spst (c:cs) tst lvl = spst cs (tst.(c:)) (lvl+1)


s2tTest t = stringToTree $ treeToString t



-- 71
-- (*) Determine the internal path length of a tree.

-- We define the internal path length of a multiway tree as the total sum of 
-- the path lengths from the root to all nodes of the tree. By this 
-- definition, tree5 has an internal path length of 9. 

ipl::Tree a->Int
ipl t = iplAux 1 t
  where
  iplAux n (Node e fr) = (length fr * n) + (sum $ map (iplAux $ n+1) fr)



-- 72
-- (*) Construct the bottom-up order sequence of the tree nodes.

-- Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up 
-- sequence of the nodes of the multiway tree Tree. 

-- Again, this function makes use of difference list strategy. Note that foldl is
-- combining the resulting difference lists of the recursion, which are
-- themselves functions, using the (.) operator.
bottomUp::Tree Char->String
bottomUp t = (buAux t) []
  where
  buAux (Node e fr) = (foldl (.) id $ map buAux fr).(e:)


-- 73
-- (**) Lisp-like tree representation.

-- There is a particular notation for multiway trees in Lisp. Lisp is a 
-- prominent functional programming language, which is used primarily for 
-- artificial intelligence problems. As such it is one of the main competitors 
-- of Prolog. In Lisp almost everything is a list, just as in Prolog 
-- everything is a term.

-- The following pictures show how multiway tree structures are represented in 
-- Lisp.

    -- visit https://wiki.haskell.org/99_questions/70B_to_73
    -- to see the picture

-- Note that in the "lispy" notation a node with successors (children) in the 
-- tree is always the first element in a list, followed by its children. The 
-- "lispy" representation of a multiway tree is a sequence of atoms and 
-- parentheses '(' and ')', which we shall collectively call "tokens". We can 
-- represent this sequence of tokens as a Prolog list; e.g. the lispy 
-- expression (a (b c)) could be represented as the Prolog list ['(', a, '(', 
-- b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which constructs the 
-- "lispy token list" LTL if the tree is given as term T in the usual Prolog 
-- notation. 

lispString::Tree Char->String
lispString t = (lsAux t) []
  where
  lsAux (Node c []) = (c:)
  lsAux (Node c fr) = ('(':).(c:).(' ':).(spcConcat $ map lsAux fr).(')':)
  spcConcat [dl] = dl
  spcConcat (dl:dls) = dl.(' ':).(spcConcat dls)


-- drop 1 cs gets rid of the space character between the root and the subtrees.
ls2tree::String->Tree Char
ls2tree [c]    = Node c []
ls2tree ('(':c:cs) = Node c (map ls2tree $ lsSubtrees $ drop 1 cs)

-- lsSubtrees receives the remaining string of a tree without it's root from
-- ls2tree and proceeds to separate it into each of it's represeted subtrees.
-- It's important to note here that drop 1 will get rid of spaces between
-- subtrees too, but it will also remove the last closing parentheses of
-- the received string, leaving a clean list of lispStrings for the
-- continuation of ls2tree recursion.
lsSubtrees::String->[String]
lsSubtrees [] = []
lsSubtrees st@('(':cs) = let (t,rem) = nestedTree st in
  t:(lsSubtrees $ drop 1 rem)
lsSubtrees (c:cs) = [c] : (lsSubtrees $ drop 1 cs)

-- nestedTree will recover the lispString of a subtree whose forest is not empty.
-- It's second string is the remainder of the string that should continue
-- recursion on lsSubtrees.
nestedTree::String->(String,String)
nestedTree st = ntAux st 0
  where
  ntAux (')':cs) 1 = (")",cs)
  ntAux (')':cs) n = cons1 ')' (ntAux cs (n-1))
  ntAux ('(':cs) n = cons1 '(' (ntAux cs (n+1))
  ntAux (c:cs) n = cons1 c (ntAux cs n)
  cons1 = \x (st1,st2)-> (x:st1,st2)

lsTest t = ls2tree $ lispString t


















