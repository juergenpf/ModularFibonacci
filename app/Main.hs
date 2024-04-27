{-
    © 2019-2024 by Jürgen Pfeifer (juergen@familiepfeifer.de)
-}
module Main where

import ModularFibonacci

experiment = take 200 fibonacciInfo

main :: IO ()
main = print experiment
{-
    The experiment shows, that 5 seems to be very special when it comes to
    Fibonacci. If you calculate the Fibonacci numbers modulo 5, the length
    of the resulting period is extraordinary. While all other periods are
    in the range of the prime±1,2*(prime±1) or even smaller than the prime, 
    the period of 5 is 20 (fibonacciWeight 5 is 5. All other primes have
    fibonacciWeight values of 1 or 2 in the experiment).

    Actually, there is mathimatical proof for these observations.
    See file assets/Fibonacci-Zahlen.pdf (in German) in this project.
    This proof was provided by Michael Becker (http://www.ijon.de/)
-}