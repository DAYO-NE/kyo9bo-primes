module Primes where

sieve :: [Int]->[Int]
sieve [] = []
sieve(nextPrime:rest) = nextPrime:sieve noFactors
 where noFactors = filter(not.(==0).(`mod`nextPrime))rest

primes :: [Int]
primes = sieve[2..10000]


isPrime :: Int -> Maybe Bool 
isPrime n | n < 2 = Nothing
  | n >= last primes = Nothing
  | otherwise = Just(n `elem` primes)
