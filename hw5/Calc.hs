{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where
import ExprT
import Parser
import qualified StackVM

-- Exercise 1
eval :: Maybe ExprT -> Integer
eval Nothing = 0
eval (Just (Lit n)) = n
eval (Just (Add e1 e2)) = eval (Just e1) + eval (Just e2)
eval (Just (Mul e1 e2)) = eval (Just e1) * eval (Just e2)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s
    | parsed == Nothing = Nothing
    | otherwise  = Just $ eval parsed
  where parsed = parseExp Lit Add Mul s

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Exercise 4
instance Expr Integer where
  lit n =  n
  add e1 e2 = e1 + e2
  mul e1 e2 = e1 * e2

instance Expr Bool where
  lit n | n > 0 = True
   | otherwise = False
  add e1 e2 = e1 || e2
  mul e1 e2 = e1 && e2

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add e1 e2 = max e1 e2
  mul e1 e2 = min e1 e2

instance Expr Mod7 where
  lit n = Mod7 ( mod n 7)
  add (Mod7 a)(Mod7 b ) = Mod7 $ mod (a + b) 7
  mul (Mod7 a)(Mod7 b ) = Mod7 $ mod (a * b) 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- Exercise 5
instance Expr StackVM.Program where
   lit n = [StackVM.PushI n]
   add [StackVM.PushI s1] [StackVM.PushI s2]= [StackVM.PushI s1, StackVM.PushI s2,StackVM.Add]
   mul [StackVM.PushI s1] [StackVM.PushI s2]= [StackVM.PushI s1, StackVM.PushI s2,StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul
