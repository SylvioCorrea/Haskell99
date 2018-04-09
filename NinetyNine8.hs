module NinetyNine8 where
import Data.List




-- 90

-- (**) Eight queens problem

-- This is a classical problem in computer science. The objective is to place 
-- eight queens on a chessboard so that no two queens are attacking each other; 
-- i.e., no two queens are in the same row, the same column, or on the same 
-- diagonal.

-- Hint: Represent the positions of the queens as a list of numbers 1..N. 
-- Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 
-- 4, the queen in the second column is in row 2, etc. Use the generate-and-test 
-- paradigm.


-- Two notes about this problem:

-- First: the algorithm description above implies we are trying to put
-- precisely eight queens on board. However, the example given in the
-- problem's page shows a function that receives the size n of the board as an
-- argument and, presumably puts n queens in such n*n board. The solution
-- provided below assumes we are attempting to put 8 queens in a traditional 8*8
-- board.
 
-- Second: This solution places queens on board in order from colum 1 to 8.
-- The exercise suggested a generate-and-test algorithm, though I thought
-- I could write one that generates only the right solutions.
-- I initially thought that as long as no subsequent queen was placed on
-- an illegal position, the board would always have room for all eight.
-- That, however, isn't true at all. There are instances where
-- 7 queens will fit perfectly, but not allow an eighth one any legal space.
-- That is, Though a placement property for legal subsequent placements was
-- easy to come up with (see safePosition function below), that is not enough to
-- prevent the algorithm from generating useless branches of computations
-- that would inevitably lack room for the last queen.
-- I couldn't come up with a property that could be checked during computation
-- of the recursion that would allow for a precise generating algorithm.
-- Therefore, if an eighth queen can't fit on the board, the last step of
-- the recursion returns an empty list (instead of a list containing
-- an empty list) imploding that branch of computation (previous results
-- waiting on the resolution of the branch in list comprehensions will
-- have nothing to work with).
eightQueens::[[Int]]
eightQueens = queensAux [] 1
  where
  queensAux previous 8     =
    let positions = [ row | row <- [1..8], safePosition row previous]
    in if null positions then [] else [positions]
  queensAux previous colum =
    [ row:rest | row <- [1..8],
      rest <- queensAux (row:previous) (colum+1),
      safePosition row previous ]


-- Function that receives the number of a row and the list of already
-- placed queens. This function assumes that one wants to place an nth queen
-- at the nth colum (all colums must have queens, since eight queens must be
-- placed on board). The queens are in proximity order, from nearest to
-- farthest colum of the next queen. That means safePosition 4 [8,3]
-- computes if a queen can be placed in the colum 3, on row 4,
-- given that there already are two queens on board, one in colum 2 and
-- row 8, and one in colum 1 and row 3
safePosition::Int->[Int]->Bool
safePosition x previous = safeAux previous 1
  where
  safeAux [] _    = True
  safeAux (p:ps) n
    |x == p   ||           -- two queens cannot be in the same row
     x == p+n ||           -- two queens cannot be in the same diagonal (up)
     x == p-n     = False  -- two queens cannot be in the same diagonal (down)
    |otherwise    = safeAux ps (n+1)




-- 91

-- (**) Knight's tour

-- Another famous problem is this one: How can a knight jump on an NxN 
-- chessboard in such a way that it visits every square exactly once? A set 
-- of solutions is given on the The_Knights_Tour page.

-- Hints: Represent the squares by pairs of their coordinates of the form 
-- X/Y, where both X and Y are integers between 1 and N. (Note that '/' is 
-- just a convenient functor, not division!) Define the relation 
-- jump(N,X/Y,U/V) to express the fact that a knight can jump from X/Y to 
-- U/V on a NxN chessboard. And finally, represent the solution of our 
-- problem as a list of N*N knight positions (the knight's tour).

-- There are two variants of this problem:

    -- 1. find a tour ending at a particular square
    -- 2. find a circular tour, ending a knight's jump from the start (clearly 
    --    it doesn't matter where you start, so choose (1,1)) 



-- Elaborating an algorithm for this problem from scratch is above my capacity.
-- All I could think of was a brute force solution. According to wikipedia's
-- page on the problem, this approach is just plain silly. There are over
-- 4*10^51 possible move sequences to compute. Through the research I
-- stumbled upon an heuristic method using the Warnsdorf rule: when given multiple
-- move options, the knight should choose to move to the square that offers the
-- least move possibilities, weeding out scenarios where the knight would fail
-- to visit every square. It seems that this approach ensues a correct path for
-- the knight with no need of movement backtracking up to a 72


-- This fundamental function takes two integers representing the position of
-- some knight in the board, a third integer n which determines the size
-- n*n of the board and returns a list with all possible places that knight
-- could reach from that square on the board.
-- It's a simple mathematical calculation that aplies sums and differences of
-- 1s and 2s to the received square coordinates withdrawing any answer that
-- would be illegal on the n*n board.
knightMoves::Int->Int->Int->[(Int,Int)]
knightMoves row colum boardSize =
  [ (f1 row n1, f2 colum n2) |
    f1 <- [(+),(-)],
    f2 <- [(+),(-)],
    (n1,n2) <- [(1,2),(2,1)],
    f1 row n1 >= 1 && f1 row n1 <= boardSize,
    f2 colum n2 >= 1 && f2 colum n2 <= boardSize ]

-- Operator (\\) is imported from Data.List and is the fuction for set
-- difference.
-- Another fundamental. This one aplies knightMoves to coordinates and
-- board size but then trims the resulting list using the set difference
-- operator combined with a fourth argument: the list of already visited
-- positions. That is, the function returns every position not already visited
-- a knight can reach from some square on the board.
possibleMoves::Int->Int->Int->[(Int,Int)]->[(Int,Int)]
possibleMoves row colum boardSize visited =
  (knightMoves row colum boardSize)\\visited
--


----------------------------------------------------------------------------
-- SOLUTION
----------------------------------------------------------------------------

-- According to one of the solutions on the wiki, using the Warnsdorf rule 
-- allows for answers on any board as high as 76*76 in size without the need 
-- of backtracking to correct failed computation branches. The algorithm I 
-- designed has no backtracking, therefore, it has this limitation (though 
-- the backtracking problem wouldn't be particularly hard to solve).
-- It chokes on 58, 62 and 70 but it solves 76 (see function ktList below).
-- I believe I've put too much trust in the heuristics and backtracking
-- support is actually mandatory.
-- I made another version of this algorithm that also takes the initial
-- square as argument (knightsTo2). Using different starting points, the
-- algorithm delivers a correct answer.
-- One page I visited cited this same problem with starting squares. Another
-- suggested also enforcing a preference for corner squares when draws in
-- available squares occur.
-- The algorithm:
-- 1. The knight always starts at (1,1). Because of this, ktAux is called 
--    from the start with a list of visited positions [(1,1)] and a triple 
--    made by (1,1, all possible moves from (1,1))
-- 2. ktAux takes the list of already visited positions and a triple 
--    containing current row and colum and all possible moves available from 
--    that position (nextList).
-- 3. lpms (least possible moves) is the function that returns the next 
--    square according to Warnsdorf rule. It takes nextList and see which one 
--    of the next possible squares has the least possible options to move from.
-- 4. Recursion ends when lpms returns Nothing (no more moves available).

-- List length comparisons occur redundantly because the algorithm uses 
-- the simple haskell linked list, but none of the compared lists will 
-- exceed length 8. Maybe the best optimization would be to use a Map or 
-- nested list for the necessary check for visited squares. That is: 
-- storing the already visited places in a Map (row as key) or nested 
-- list (working like a matrix of linked lists). The only problem would 
-- be that the order in which the squares are visited would also have to 
-- be stored in said structures using an extra integer.

knightsTo::Int->[(Int,Int)]
knightsTo n = ktAux [(1,1)] (1,1,possibleMoves 1 1 n [(1,1)])
  where
  ktAux visited (row, colum, nextList) =
    case lpms nextList of
    Nothing                        -> visited
    Just (newRow,newColum,newNext) -> ktAux ((newRow,newColum):visited) (newRow,newColum,newNext)
    where
    -- Finds which of the available squares has the least possible moves available from
    lpms moves = foldr lpmsAux Nothing moves
      where
      lpmsAux (r,c) res = lowest (Just (r,c,pms)) res
        where
        pms = possibleMoves r c n visited
        lowest whatever Nothing = whatever
        lowest (Just (r1,c1,l1)) (Just (r2,c2,l2))
          |length l1 < length l2 = (Just (r1,c1,l1))
          |otherwise             = (Just (r2,c2,l2))
          
----------------------------------------------------------------------------

ktTest n = (n, n*n == length (knightsTo n))
ktList = map ktTest [n | n <- [8..76], n `mod` 2 == 0] 


knightsTo2::Int->Int->Int->[(Int,Int)]
knightsTo2 sr sc n = ktAux [(sr,sc)] (sr,sc,possibleMoves sr sc n [(sr,sc)])
  where
  ktAux visited (row, colum, nextList) =
    case lpms nextList of
    Nothing                        -> visited
    Just (newRow,newColum,newNext) -> ktAux ((newRow,newColum):visited) (newRow,newColum,newNext)
    where
    -- Finds which of the available squares has the least possible moves available from
    lpms moves = foldr lpmsAux Nothing moves
      where
      lpmsAux (r,c) res = lowest (Just (r,c,pms)) res
        where
        pms = possibleMoves r c n visited
        lowest whatever Nothing = whatever
        lowest (Just (r1,c1,l1)) (Just (r2,c2,l2))
          |length l1 < length l2 = (Just (r1,c1,l1))
          |otherwise             = (Just (r2,c2,l2))














