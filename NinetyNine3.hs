module NinetyNine3 where
import Data.List
import qualified Data.Map.Lazy as M

{- 46
  Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 
  (for logical equivalence) which succeed or fail according to the 
  result of their respective operations; e.g. and(A,B) will succeed, if 
  and only if both A and B succeed.

  A logical expression in two variables can then be written as in the 
  following example: and(or(A,B),nand(A,B)).

  Now, write a predicate table/3 which prints the truth table of a 
  given logical expression in two variables.
-}

booleans = [True, False]

type Predicate = (Bool->Bool->Bool)

and2::Predicate
and2 True True = True
and2 _ _       = False

or2::Predicate
or2 True _ = True
or2 _ True = True
or2 _ _    = False

nand::Predicate
nand = \p q -> not $ and2 p q

nor::Predicate
nor = \p q -> not $ or2 p q

xor::Predicate
xor True False = True
xor False True = True
xor _ _        = False

impl::Predicate
impl True False = False
impl _ _        = True

equ::Predicate
equ True True   = True
equ False False = True
equ _ _         = False

table::Predicate->IO()
table pred =
  putStr $ concat
    [ (show p) ++" "++
      (show q) ++" "++
      (show $ pred p q) ++ "\n" | p <- booleans, q <- booleans ]

--47
infixl 5 `equ`
infixl 4 `and2`
infixl 4 `nand`
infixl 3 `or2`
infixl 3 `nor`
infixl 3 `xor`
infixl 2 `impl`

--48
-- Truth table for any number of variables
tablen::Int->([Bool]->Bool)->IO()
tablen n pred =
  putStr $ concat
    [ (concat $ map (\x -> (show x) ++ " ") xs) ++
      (show $ pred xs) ++ "\n" | xs <- tableBuild n ]

tableBuild::Int->[[Bool]]
tableBuild 0 = [[]]
tableBuild n = [ x:xs | x <- booleans, xs <- tableBuild $ n-1 ]

expr1 = (\[p, q, r] -> p `and2` q `and2` r)
test301 = tablen 3 expr1

--49
-- Gray Codes
gray::Int->[String]
gray 0 = [""]
gray n = [ '0':xs | xs <- gray'] ++ [ '1':xs | xs <- reverse gray']
  where
  gray' = gray $ n-1

isGrayCode::Int->[String]->Bool
isGrayCode 0 []  = True
isGrayCode _ [c] = False
isGrayCode n cs  = (length cs == 2^n) && (grayCheck $ cs ++ [(head cs)])

grayCheck::[String]->Bool
grayCheck [c1,c2]      = spotDiff c1 c2 == 1
grayCheck (c1:c2:cs)
  |spotDiff c1 c2 == 1 = grayCheck (c2:cs)
  |otherwise           = False

-- Auxiliary function that returns the number of different elements
-- of same index in two lists. A Gray Code has at most one difference
-- between two consecutive binary codes.
spotDiff::String->String->Int
spotDiff [] [] = 0
spotDiff [] ys = 2
spotDiff xs [] = 2
spotDiff (x:xs) (y:ys)
  |x==y      = 0 + (spotDiff xs ys)
  |otherwise = 1 + (spotDiff xs ys)



--50
-- Huffman codes.

-- ========================================================
data HuffmanNode a = HN {codedList::[(a,String)],
                         frequency::Int
                         } deriving Show

freqList2HuffList::[(v,Int)]->[HuffmanNode v]
freqList2HuffList xs = [HN [(v,"")] f| (v,f) <- xs]


toLeft::HuffmanNode a -> HuffmanNode a
toLeft (HN xs f) = HN ( map ( \(v,c) -> (v,'0':c) ) xs ) f

toRight::HuffmanNode a -> HuffmanNode a
toRight (HN xs f) = HN ( map ( \(v,c) -> (v,'1':c) ) xs ) f

hnJoin::HuffmanNode a -> HuffmanNode a -> HuffmanNode a
hnJoin hn1 hn2 =
  HN (codedList hn1 ++ codedList hn2) (frequency hn1 + frequency hn2)

encodeJoin::HuffmanNode a -> HuffmanNode a -> HuffmanNode a
encodeJoin hn1 hn2 = (toLeft hn1) `hnJoin` (toRight hn2)
-- ========================================================

  
huffmanCode::String->[(Char,String)]
huffmanCode [] = []
huffmanCode st = encode (huffmanStart $ frequencyMap st) []
  where
  encode [hn] [] = codedList hn
  encode [] [hn] = codedList hn
  encode xs ys = encode xs' $ ys'++[(encodeJoin hn1 hn2)]
    where
    (hn1, hn2, xs', ys') = get2Lowest xs ys


getLowest::[HuffmanNode a]->[HuffmanNode a]->(HuffmanNode a,[HuffmanNode a],[HuffmanNode a])
getLowest [] (h:t) = (h,[],t)
getLowest (h:t) [] = (h,t,[])
getLowest l1@(h1:t1) l2@(h2:t2)
  |frequency h1 < frequency h2 = (h1,t1,l2)
  |otherwise                   = (h2,l1,t2)

get2Lowest xs ys = (hn1, hn2, xs'', ys'')
  where
  (hn1,xs'',ys'') = getLowest xs' ys'
  (hn2,xs',ys')   = getLowest xs ys

frequencyMap::String->M.Map Char Int
frequencyMap [] = M.empty
frequencyMap (x:xs) = M.insertWith (+) x 1 (frequencyMap xs)

huffmanStart::M.Map Char Int->[HuffmanNode Char]
huffmanStart freqMap =
  freqList2HuffList $ quicksortByValue (M.toList freqMap)


quicksortByValue::(Ord v)=>[(k,v)]->[(k,v)]
quicksortByValue [] = []
quicksortByValue ((hk,hv):t) =
  (quicksortByValue smaller)++[(hk,hv)]++(quicksortByValue bigger)
    where
    (smaller,bigger) = partition (\(k,v) -> v <= hv) t









