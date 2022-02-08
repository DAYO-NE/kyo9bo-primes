module Primes where

isPrime :: Int -> Maybe Bool 
isPrime n | n < 0 = Nothing
  | n >= last primes = Nothing
  | otherwise = Just (n `elem` primes)

primes :: [Int]
primes = [1..10000]


sieve :: [Int]->[Int]
sieve [] = []
sieve(nextPrime : rest) = nextPrime : sieve noFactors
 where noFactors = filter(not.(==0).(`mod`nextPrime)) rest


