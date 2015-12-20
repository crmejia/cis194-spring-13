module Golf where
import Data.List

-- Exercise 1 Hopscotch
skips :: [a] -> [[a]]
skips [] = []
-- add the input as the 1st element of the result
-- then call the function makeSkipList to add a list of list of every other skipped element
skips xs =  [xs] ++ makeSkipList 1 xs

-- call everyOther recursively for every element in the input list from n..(length of input)
makeSkipList :: Int -> [a] -> [[a]]
makeSkipList n xs
  | n == (length xs) = []
  |otherwise  = [everyOther n xs] ++ (makeSkipList (n+1) xs)

--take everyother 'n' elements from a list
-- drop recursively n elements and take the head of the list
-- then shorten the list by dropping twice n and call again
everyOther :: Int -> [a] -> [a]
everyOther n xs
  | xsLength > n =  head (drop n xs) : everyOther n (drop (n+n) xs)
  | xsLength == n && n `mod` 2 == 0 = head (drop 1 xs) :[]
  | otherwise = []
    where xsLength = length xs

-- Exercise 2 Local Maxima
-- [2,3,4,1,5] == [4]
localMaxima::[Integer]->[Integer]
localMaxima [] = []
localMaxima (x1:[]) = []
localMaxima (x1:x2:[]) = []
localMaxima (x1:x2:x3:xs)
  | isLocalMaxima x1 x2 x3 = x2 : localMaxima (x2:x3:xs)
  | otherwise              = localMaxima (x2:x3:xs)

isLocalMaxima x1 x2 x3 = x2 > x3 && x2 > x1

-- Exercise 3 histogram
histogram :: [Integer] -> String
xAxis = "==========\n0123456789\n"
histogram [] = xAxis
 -- The lenght being the max amount possible of repetitions as in the whole list being the same number
histogram xs = (writeHistogram (length xs) xs) ++ xAxis

-- map the lenght of the list to the repetition. The lenght being the max amount possible of repetitions
-- the list is reversed to draw backwards from the top
-- histogram xs = map draw (reverse [0..(lenght xs)])
--
writeHistogram :: Int -> [Integer] -> String
writeHistogram _ [] = []
writeHistogram 0 _ = []
writeHistogram depth xs = (writeLine 0 depth xs) ++ (writeHistogram (depth - 1) xs)

--write line for histogram for depth n
--so if n = 5 see if any integer in the input list repeats 5 times if it does
-- then add a * and keep going though the integers
-- is there a way to get rid of 'index' as an argument
writeLine :: Integer -> Int -> [Integer] -> String
writeLine _ _ [] = []
writeLine index depth xs
    | index < 10   = smallIndex
    | otherwise    = "\n"
  where
    smallIndex
      | length (elemIndices index xs) == depth  = "*" ++ writeLine (index + 1) depth xs
      | otherwise                               = " " ++ writeLine (index + 1) depth xs
