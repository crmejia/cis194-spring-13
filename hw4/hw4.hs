import qualified Data.List as L
-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter(even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
 | even n = n + fun2 (n `div` 2)
 | otherwise = fun2 (3 * n + 1)

-- fun2' :: Integer -> Integer

-- Exercise 2: Folding with trees
data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
-- foldTree [] = Leaf
foldTree xs = foldr insert Leaf xs

-- insert in a way that the tree remains balanced
insert ::  a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node height left root right)
    | heightLeft < heightRight  = Node height (insert a left) root right
    | heightLeft > heightRight  = Node height left root (insert a right)
    | otherwise = Node (heightRight + 1) left root (insert a right)
  where
    heightLeft = heightTree left
    heightRight = heightTree right
    heightTree Leaf = 0
    heightTree (Node h _ _ _) = h

-- Exercise 3

xor::[Bool]->Bool
xor [] = False
xor (x:xs) = foldr logic x xs
-- (\x acc -> | x == acc = False | otherwise tre)

logic :: Bool -> Bool -> Bool
logic x acc
  | x == acc = False
  | otherwise = True

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- Exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
-- 1st generate a list of all results from i+j+2ij. Using cartprod to generate a list of all pairs (i,j).
--        also remove duplicates with nub
-- 2nd filter on the condition <n and get the list difference(\\, whatever is not marked by the sieve)
        -- between the sieve result and [1..n]
-- 3rd finally maps the remaining elements to 2k+1

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\k->2*k+1) . (L.\\) [1..n] . filter (<=n) . L.nub . map (\(i,j)->i+j+2*i*j) $ cartProd [1..n] [1..n]
