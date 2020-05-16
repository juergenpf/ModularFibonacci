{-
  © 2019-2020 by Jürgen Pfeifer (juergen@familiepfeifer.de)
  
  The ModularFibonacci module implements some functions to compute some properties of 
  the Fibonacci number series calculated with modular arithmetic.
-}
module ModularFibonacci
    ( primes,
    fibonacci,
    fibonacciModP,
    fibonacciModP',
    fibonacciPeriod,
    fibonacciInfo
    ) where

import Data.Ratio

{-| 
  A simple definition to generate the infinite list of primes
  Good enough for experimenting with "small" primes
-}
primes :: [Integer]
primes = sieve (2 : [3, 5..])
  where
    sieve (p:xs) = p : sieve [ x|x <- xs, x `mod` p > 0 ]

{-| 
  The classical infinite series of integer Fibonacci numbers 
-}
fibonacci :: [Integer]
fibonacci = fibs
    where
      fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{-| 
  The fibonacciModP function calculates the infinite list of fibonacci numbers modulo a Prime p.
  It takes one argument, which is the (Prime) number p.
-}
fibonacciModP :: Integer -> [Integer]
fibonacciModP p = fibsp
  where
    plus p x y = (x + y) `mod ` p
    fibsp = 0 : 1 : zipWith (plus p) fibsp (tail fibsp)

{-| 
    The fibonacciModP' function just returns the finite list with the first period of the Fibonacci series modulo a Prime number p.
    It takes one argument, which is the (Prime) number p.
-}
fibonacciModP' :: Integer -> [Integer]
fibonacciModP' p = take (period!!0) fibs
  where
    fibs = fibonacciModP p
    period = [ n | n <- [2,3..], fibs!!(n-1)==1, fibs!!(n)==0 ]

{-| 
  The fibonacciPeriod function calculates the length of a period of fibonacci numbers modulo a prime.
  It takes one argument, which is the (Prime) number p.
-}
fibonacciPeriod :: Integer -> Integer
fibonacciPeriod p = toInteger (length (fibonacciModP' p))

{-|              
  An infinite list of some characteristics for Fibonacci series calculated with modular arithmetic with a prime modulus. 
  The characteristics are 4-tuples containing 
     [ the prime itself, the fibonacciPeriod, the fibonacciModulus, the fibonacciWeight ].
  The terms fibonacciModulus and fibonacciWeight are defined in the function.
-}
fibonacciInfo :: [(Integer,Integer,Integer,Integer)]
fibonacciInfo = [ (p,fPeriod,fibonacciModulus,fibonacciWeight) | p <- primes, 
    let fPeriod = fibonacciPeriod p,
    -- We call this the FibonacciModulus of prime p
    let fibonacciModulus = max (gcd (p+1) fPeriod) (gcd (p-1) fPeriod),
    -- We call this the FibonacciWeight of prime p
    let fibonacciWeight = fPeriod `div` fibonacciModulus ]
