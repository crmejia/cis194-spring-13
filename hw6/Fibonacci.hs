module Fibonacci
    (
    ) where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = scanl (\acc n -> fib(n-2) + acc) 1 [2..]

--Exercise 3
data Stream a = SCons a (Stream a)

instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))

streamToList :: Stream a -> [a]
streamToList (SCons a s) = a : streamToList s

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = SCons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (SCons a s) = SCons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = SCons seed (streamFromSeed f (f seed))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- ruler :: Stream Integer

interleaveStreams :: Stream Integer -> Stream Integer -> Stream Integer
interleaveStreams (SCons m s1) s2 = SCons m (interleaveStreams s2 s1)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) withOnes
  where
    divisible = interleaveStreams (streamRepeat 2) (streamFromSeed (+1) 3)
    withOnes = interleaveStreams (streamRepeat 1) divisible
