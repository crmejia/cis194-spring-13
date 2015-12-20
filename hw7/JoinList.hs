{-# LANGUAGE FlexibleContexts #-}
module JoinList where

import Data.Monoid
import Sized

data JoinList m a =
  Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
-- tag (Append m _ _) = m
-- according to the fingertree article the tag should be computed like this
tag (Append _ jl1 jl2) = mappend (tag jl1) (tag jl2)
tag Empty = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
-- is the calculation of the append made redundandt by the tag calculation
(+++) jl1 jl2 = Append (mappend (tag jl1) (tag jl2)) jl1 jl2

 -- Exercise 2
 -- (1)

-- indexJ _ Empty = Nothing
-- indexJ index (Append b jl1 jl2)
--   | index < (getSize (size b)) = indexJ index jl1
--   | otherwise = Nothing
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index jl
  | index < 0 = Nothing
  | gt (sizeJl jl) = Just (search mempty jl)
  | otherwise = Nothing
  where
    search acc (Single _ a) = a
    search acc (Append _ jl1 jl2)
      | gt (mappend mempty jl1) = search acc jl1
      | otherwise = search (mappend mempty jl1) jl2
    sizeJl (Append m _ _) = getSize (size m)
    sizeJl (Single m _) = getSize (size m)
    gt = (>) index
