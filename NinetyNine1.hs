module NinetyNine1 where
import System.Random
import Data.List
import qualified Data.Map.Lazy as Map

--Part one of 99 Haskell problems: 28 problems

--listas para testes
list1 = [0..10]
list2 = "abcde"
list3 = "abc"
list4 = ['a'..'k']
list5 = ['a'..'r']

--1
myLast::[a]->a
myLast [x] = x
myLast (h:t) = myLast t

--2
myButLast::[a]->a
myButLast (x:[x']) = x
myButLast (h:t) = myButLast t

--3
elementAt::[a]->Int->a
elementAt (h:t) 1 = h
elementAt (h:t) n = elementAt t (n-1)

--4
myLength::[a]->Int
myLength [] = 0
myLength (h:t) = 1 + (myLength t)

--5
myReverse::[a]->[a]
myReverse xs = myReverse' xs []
  where
  myReverse' [] res = res
  myReverse' (h:t) res = myReverse' t (h:res)
  
--6
isPalindrome::(Eq a)=>[a]->Bool
isPalindrome xs = xs == (myReverse xs)

--7
data NestedList a = Elem a | List [NestedList a] deriving Show

nested = List [Elem 1, Elem 2, Elem 3, nested2, Elem 6, nested3, Elem 9]
nested2 = List [Elem 4, Elem 5]
nested3 = List [Elem 7, List [Elem 8]]

flatten::NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = flatten' xs
  where
  flatten' [] = []
  flatten' ( (Elem y)  : t ) = y : (flatten' t)
  flatten' ( (List ys) : t ) = (flatten' ys) ++ (flatten' t)

--8
compress::(Eq a)=>[a]->[a]
compress []    = []
compress (h:t) = h : (compressAux t)
  where
  compressAux [] = []
  compressAux (h2:t2)
    |h==h2     = compressAux t2
    |otherwise = compress (h2:t2)

--9
pack::(Eq a)=>[a]->[[a]]
pack [] = []
pack (h:t) = packAux [h] t
  where
  packAux eqs [] = [eqs]
  packAux eqs (h2:t2)
    |h==h2     = packAux (h2:eqs) t2
    |otherwise = eqs : (pack (h2:t2))

--10
encode::(Eq a)=>[a]->[(Int, a)]
encode [] = []
encode (h:t) = encodeAux 1 t
  where
  encodeAux n [] = [(n,h)]
  encodeAux n (h2:t2)
    |h==h2     = encodeAux (n+1) t2
    |otherwise = (n,h) : (encode (h2:t2))


--11
data Encoded a = Single a | Multiple Int a deriving Show

encodeModified::(Eq a)=>[a]->[Encoded a]
encodeModified [] = []
encodeModified (h:t) = encodeModifiedAux 1 t
  where
  encodeModifiedAux n [] = [encodeData n h]
  encodeModifiedAux n (h2:t2)
    |h==h2     = encodeModifiedAux (n+1) t2
    |otherwise = (encodeData n h) : (encodeModified (h2:t2))

encodeData::Int->a->Encoded a
encodeData 1 x = Single x
encodeData n x = Multiple n x

--12
decodeModified::[Encoded a] -> [a]
decodeModified [] = []
decodeModified ( (Single x)     : t ) = x : (decodeModified t)
decodeModified ( (Multiple n x) : t ) = fastRepeat n x (decodeModified t)

fastRepeat::Int->a->[a]->[a]
fastRepeat 0 x xs = xs
fastRepeat n x xs = x : (fastRepeat (n-1) x xs)

--13
-- ???

--14
dupli::[a]->[a]
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

--15
repli::[a]->Int->[a]
repli [] _     = []
repli (x:xs) n = fastRepeat n x (repli xs n)

--16
dropN::[a]->Int->[a]
dropN [] _ = []
dropN (x:xs) 1 = xs
dropN (x:xs) n = x : (dropN xs $ n-1)

--17
mySplit::[a]->Int->([a],[a])
mySplit [] _ = ([],[])
mySplit (x:xs) 1 = ([x],xs)
mySplit (x:xs) n = (x : xs1, xs2)
  where
  (xs1, xs2) = mySplit xs (n-1)

--18
--requer 0 < start <= end 
slice::[a]->Int->Int->[a]
slice [] _ _           = []
slice (x:xs) 1 1       = [x]
slice (x:xs) 1 end     = x : (slice xs 1 $ end-1)
slice (x:xs) start end = slice xs (start-1) (end-1)

--19
rotate::[a]->Int->[a]
rotate [] _     = []
rotate xs n 
  |n `mod` lnt == 0 = xs
  |otherwise        = rotate' xs (n `mod` lnt) id
  where
  rotate' ys 0 fc      = ys++(fc [])
  rotate' (y:ys) n' fc = rotate' ys  (n'-1) (fc.(y:))
  lnt = length xs
  
--20
--requires length list >= n > 0
removeAt::[a]->Int->(a,[a])
removeAt (x:xs) 1 = (x, xs)
removeAt (x:xs) n = case removeAt xs (n-1) of
                    (e,xs') -> (e, x:xs')
--21
--requires length list <= n+1, n > 0
insertAt::a->[a]->Int->[a]
insertAt e xs 1     = e:xs
insertAt e (x:xs) n = x : (insertAt e xs $ n-1)

--22
--solução que inclui casos em que a>b colada da lista de soluções
range::Int->Int->[Int]
range a b
  |a==b = [a]
  |a<b  = a : (range (a+1) b)
  |a>b  = a : (range (a-1) b)

--23 
{-
rndSelect::[a]->Int->IO [a]
rndSelect xs 0 = return []
rndSelect xs n = 
  do randomInt <- rangeRandom 0 $ length xs - 1
     list      <- rndSelect xs (n-1)
     return $ (xs!!randomInt) : list
-}
rangeRandom::Int->Int->IO Int
rangeRandom lo hi = getStdRandom $ randomR (lo,hi)

--This solution chooses any given element of the list only once by
--using the previously implemented solution from problem 20 (removeAt).
--If the desired number of elements is larger then the list length
--the entire list is returned with unspecified order.
rndSelect::[a]->Int->IO [a]
rndSelect xs 0 = return []
rndSelect xs n = 
  do randomInt  <- rangeRandom 0 $ length xs - 1
     let (e,ys) = removeAt xs (randomInt + 1)
     fmap (e:) (rndSelect ys $ n-1)

--24
--This solution uses rndSelect from the previous problem
diffSelect::Int->Int->IO[Int]
diffSelect n m = rndSelect [1..m] n

--25
--This solution uses rndSelect from problem 23
rndPermutation::[a]->IO[a]
rndPermutation xs = rndSelect xs $ length xs

--26
-- This solution is wrong!
--Requires list length >= n >= 0
{-
combinations::Int->[a]->[[a]]
combinations 0 _ = []
combinations 1 xs = [ [x] | x <- xs]
combinations n xs = [ x:y | x <- xs, y <- combinations (n-1) xs]
-}

-- Like a power set of the given list, but without the empty set.
combineAll::[a]->[[a]]
combineAll [] = []
combineAll [x] = [[x]]
combineAll (x:xs) = [x]:[ x:ys | ys <- yss] ++ yss
  where
  yss = combineAll xs



--27
infinite xs = xs++infinite xs
--Multinomial coefficient
group::[Int]->[a]->[[[a]]]
group partitionArgs xs
  |illegalArgs = error "Illegal arguments."
  |otherwise   = multinomialC partitionArgs xs
  where
  illegalArgs = partitionArgs == [] ||
                sum partitionArgs /= length xs ||
                elem 0 partitionArgs 
                
{-
multinomialC::[Int]->[a]->[[[a]]]
multinomialC [] _      = []
multinomialC (n:ns) xs = aux n infnt
  where
  aux 0 ys    = []
  aux iter ys = [(take n zs1):m | m <- multinomialC ns zs2] ++ [(aux (iter-1) (drop 1 ys))]
  (zs1,zs2) = splitAt n infnt
  infnt = infinite xs
-}

multinomialC::[Int]->[a]->[[[a]]]
multinomialC (n:ns) xs = mltnAux (length xs) xs
  where
  mltnAux 0 _     = []
  mltnAux iter ys =
    case splitAt n ys of
    (zs1,[])  -> [[zs1]]
    (zs1,zs2) -> [zs1 : m | m <- multinomialC ns zs2] ++ mltnAux (iter-1) shiftedYs
    where
    (zs1,zs2) = splitAt n ys
    shiftedYs = slice (infinite ys) 2 $ length ys + 1

--28
--Schwartzian transform approach (decorate-sort-undecorate)
--a)
data SizedList a = SizedList {list::[a],
                              size::Int
                              } deriving Show

-- Cons
infixl 1 .:.
(.:.)::a->SizedList a->SizedList a
x .:. (SizedList xs n) = SizedList (x:xs) (n+1)

-- Append
infixr 1 .++.
(.++.)::SizedList a->SizedList a->SizedList a
(SizedList xs n).++.(SizedList ys m) = SizedList (xs++ys) (n+m)

toSizedList::[a]->SizedList a
toSizedList xs = SizedList xs (length xs)

orderByLength::[[a]]->[[a]]
orderByLength xs = map list (quicksortSL (map toSizedList xs))

quicksortSL::[SizedList a]->[SizedList a]
quicksortSL [] = []
quicksortSL (h:t) = (quicksortSL smaller)++[h]++(quicksortSL bigger)
  where
  (smaller,bigger) = partition (\sl -> size sl <= size h) t

--b)
orderByRarity::[[a]]->[[a]]
orderByRarity xs = map list (quicksortR sl rMap)
  where
  sl = map toSizedList xs
  rMap = rarityMap sl

rarityMap::[SizedList a]->Map.Map Int Int
rarityMap xs = rarityMapAux xs Map.empty
  where
  rarityMapAux [] rMap     = rMap
  rarityMapAux (y:ys) rMap =
    rarityMapAux ys (Map.insertWith (+) (size y) 1 rMap)

quicksortR::[SizedList a]->Map.Map Int Int->[SizedList a]
quicksortR [] rMap    = []
quicksortR (h:t) rMap = (quicksortR smaller rMap)++[h]++(quicksortR bigger rMap)
  where
  (smaller,bigger) = partition (\sl-> rarity sl <= rarity h) t
  rarity sl' = case Map.lookup (size sl') rMap of
              Just r  -> r
              Nothing -> error "Key not found."

--listas para teste
testList = [list2, list3, list4, list5, list5, list2, list2, list5, list4]
slist1 = map toSizedList testList
rMapSlist = rarityMap slist1













