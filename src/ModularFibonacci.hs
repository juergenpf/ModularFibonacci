-- © 2019-2020 by Jürgen Pfeifer (juergen@familiepfeifer.de)
--
-- Compute some characteristical information for the Fibonacci number
-- series calculated with modular arithmetic.

module ModularFibonacci
    ( someFunc, 
    primes,
    fibonacci,
    fibonacciModP,
    fibonacciModP',
    fibonacciPeriod,
    fibonacciInfo
    ) where

import Data.Ratio

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- A simple definition to generate the infinite list of primes
-- Good enough for experimenting with "small" primes    
primes :: [Integer]
primes = sieve (2 : [3, 5..])
  where
    sieve (p:xs) = p : sieve [ x|x <- xs, x `mod` p > 0 ]

-- The classical infinite series of integer Fibonacci numbers
fibonacci :: [Integer]
fibonacci = fibs
    where
      fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Generate the infinite Fibonacci numbers list modulo p
-- e.g. fibonacciModP 3 produces [0,1,1,2,0,2,2,1,0,1,1,2,0,2,2,1,0,1,..]
fibonacciModP :: Integer -> [Integer]
fibonacciModP p = fibsp
  where
    plus p x y = (x + y) `mod ` p
    fibsp = 0 : 1 : zipWith (plus p) fibsp (tail fibsp)

-- Fibonacci numbers modulo p are periodic lists
-- This function just returns the finite list with the first period
-- e.g. fibonacciModP' 3 produces [0,1,1,2,0,2,2,1]
fibonacciModP' :: Integer -> [Integer]
fibonacciModP' p = take (period!!0) fibs
  where
    fibs = fibonacciModP p
    period = [ n | n <- [2,3..], fibs!!(n-1)==1, fibs!!(n)==0 ]

-- We define the "FibonacciPeriod" of a (Prime) number p as the
-- length of the period of Fibonacci numbers modulo p
-- e.q. fibonacciPeriod 3 is 8
fibonacciPeriod :: Integer -> Integer
fibonacciPeriod p = toInteger (length (fibonacciModP' p))
              
-- An infinite list of some characteristics for Fibonacci series 
-- calculated with modular arithmetic with a prime modulus.
-- The characteristics are 4-tuples containing 
--    the prime itself, 
--    the fibonacciPeriod, 
--    the fibonacciModulus
--    the fibonacciWeight
-- The terms fibonacciModulus and fibonacciWeight are defined in
-- the function 
fibonacciInfo :: [(Integer,Integer,Integer,Integer)]
fibonacciInfo = [ (p,fPeriod,fibonacciModulus,fibonacciWeight) | p <- primes, 
    let fPeriod = fibonacciPeriod p,
    -- We call this the FibonacciModulus of prime p
    let fibonacciModulus = max (gcd (p+1) fPeriod) (gcd (p-1) fPeriod),
    -- We call this the FibonacciWeight of prime p
    let fibonacciWeight = fPeriod `div` fibonacciModulus ]
