module Ficha1 where

import CP
import Data.Maybe

-- (2)

length' :: [a] -> Int
length' = foldr (\x -> (+) 1) 0

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- (3)

catMaybes' :: [Maybe a] -> [a]
catMaybes' = map fromJust . filter isJust

-- (4)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x, y) = f x y

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- (5)

data LTree a = Leaf a
             | Fork (LTree a, LTree a)

flatten :: LTree a -> [a]
flatten (Leaf a) = [a]
flatten (Fork (x, y)) = flatten x ++ flatten y

mirror :: LTree a -> LTree a
mirror (Leaf a) = Leaf a
mirror (Fork (x, y)) = Fork (y, x)

fmap' :: (b -> a) -> LTree b -> LTree a
fmap' f (Leaf b) = Leaf (f b)
fmap' f (Fork (x, y)) = Fork (fmap' f x, fmap' f y)

