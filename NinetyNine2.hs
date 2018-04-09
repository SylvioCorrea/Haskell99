module NinetyNine2 where

--31
isPrime::Integer->Bool
isPrime n |n<2       = False
          |otherwise = isPrimeAux n (floor $ sqrt $ fromInteger n)
  where
  isPrimeAux n m |m<2            = True
                 |n `mod` m == 0 = False
                 |otherwise      = isPrimeAux n (m-1)

--32
-- (**) Determine the greatest common divisor of two positive integer numbers.
-- Use Euclid's algorithm.
myGCD::Integer->Integer->Integer
myGCD n m = euclid n m (min n m)
  where
  euclid n' m' gcd
    |n'==0 || m'==0 = gcd
    |n'>=m'       = euclid (n'-m') m' m'
    |n'<m'        = euclid n' (m'-n') n'

--33
-- (*) Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1. 
coprime::Integer->Integer->Bool
coprime n m = myGCD n m == 1

--34
-- (**) Calculate Euler's totient function phi(m).
totient::Integer->Integer
totient 1 = 1
totient n = totientAux 1
  where
  totientAux m
    |m==n          = 0
    |m `coprime` n = 1 + totientAux (m+1)
    |otherwise     = totientAux (m+1)

--35
-- (**) Determine the prime factors of a given positive integer.
-- Construct a flat list containing the prime factors in ascending order. 
primeFactors::Integer->[Integer]
primeFactors n
  |n<2       = []
  |otherwise = [x | x <- [2..n `div` 2], isPrime x, n `mod` x == 0]

--36
-- (**) Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity. 
primeFactorsMult::Integer->[(Integer,Integer)]
primeFactorsMult n = aux (primeFactors n)
  where
  aux []     = []
  aux (p:ps) = (p, (multPrime n p)) : (aux ps)

multPrime::Integer->Integer->Integer
multPrime n p = multPrimeAux n
  where
  multPrimeAux m
    |m==0           = 0
    |m `mod` p /= 0 = 0
    |otherwise      = 1 + multPrimeAux (m `div` p)

--37
-- (**) Calculate Euler's totient function phi(m) (improved).
phi::Integer->Integer
phi n = phiAux (primeFactorsMult n)
  where
  phiAux [] = 1
  phiAux ((p,m):t) = (p-1) * p^(m-1) * phiAux t

--38
-- (*) Compare the two methods of calculating Euler's totient function.
-- no solution required

--39
-- (*) A list of prime numbers.
-- Given a range of integers by its lower and upper limit,
-- construct a list of all prime numbers in that range. 
primesR::Integer->Integer->[Integer]
primesR n m = [ x | x <- [n..m], isPrime x]

--40
-- (**) Goldbach's conjecture.
-- Goldbach's conjecture says that every positive even number greater
-- than 2 is the sum of two prime numbers. Example: 28 = 5 + 23.
-- It is one of the most famous facts in number theory that has not been
-- proved to be correct in the general case.
goldbach::Integer->(Integer,Integer)
goldbach n = goldbachAux primes (reverse primes)
  where
  primes = primesR 2 n
  goldbachAux (p:ps) (rp:rps) = case compare (p+rp) n of
                                     EQ -> (p,rp)
                                     GT -> goldbachAux (p:ps) rps
                                     LT -> goldbachAux ps (rp:rps)

-- 41
-- Given a range of integers by its lower and upper limit, print a
-- list of all even numbers and their Goldbach composition. 
goldbachList::Integer->Integer->[(Integer,Integer)]
goldbachList n1 n2 = [goldbach x | x <- [n1..n2], x `mod` 2 == 0]

-- In most cases, if an even number is written as the sum of two prime
-- numbers, one of them is very small. Very rarely, the primes are both
-- bigger than say 50. Try to find out how many such cases there are in
-- the range 2..3000. 
goldbachList'::Integer->Integer->Integer->[(Integer,Integer)]
goldbachList' n1 n2 min =
  [ (x,y) | (x,y) <- goldbachList n1 n2, x > min && y > min]
