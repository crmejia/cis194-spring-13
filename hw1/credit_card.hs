-- Ex 1
toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
 -- Ex 2
doubleEveryOther :: [Integer] -> [Integer]
-- Ex 3
sumDigits :: [Integer] -> Integer
-- Ex 4
validate :: Integer -> Bool

toDigits n
  | n <= 0            = []
  | n `mod` 10 == n   =  [n]
  -- | otherwise         = n `mod` 10 : toDigits (div n 10)
  | otherwise         = toDigits (div n 10)  ++ [n `mod` 10]

toDigitsRev n = reverse(toDigits n)

-- caveat this function assumes the list is already reversed
doubleEveryOther []     = []
doubleEveryOther (x:[]) = [2 * x]
doubleEveryOther (x:y:zs) = x : 2 * y : doubleEveryOther zs

sumDigits []            = 0
sumDigits (x:xs) = mod x 10 + div x 10 + sumDigits xs

{- Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
If the result equals 0, then the number is valid. -}

validate n
  | sumDigits(doubleEveryOther(toDigitsRev n)) `mod` 10 == 0  = True
  | otherwise                                               = False
