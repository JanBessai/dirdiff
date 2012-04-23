module Data.Tree.Diff 
  ( Difference(..)
  , treeDifference
  ) where

import Data.List
import Data.Tree
import Data.Function
import Data.Maybe
import Control.Applicative

equalRoots :: (Eq a) => Tree a -> Tree a -> Bool
equalRoots = (==) `on` rootLabel

data Difference a =
  First a
  | Second a
  | Both a
  deriving (Show, Eq, Ord)


mergeTrees :: (Eq a) => Tree a -> Tree a -> Forest (Difference a)
mergeTrees (Node x xs) (Node y ys)
 | x == y =
   [ Node (Both x) ( (concat $ zipWith mergeTrees
                               (intersectBy equalRoots xs ys)
                               (intersectBy equalRoots ys xs))
                     ++ map (fmap First)  (deleteFirstsBy equalRoots xs ys)
                     ++ map (fmap Second) (deleteFirstsBy equalRoots ys xs))
   ]
 | otherwise =
   [ Node (First x) (map (fmap First) xs)
   , Node (Second y) (map (fmap Second) ys)]

filterCommon :: (Eq a) => Tree (Difference a) -> Maybe (Tree (Difference a))
filterCommon (Node (Both x) xs) =
  case catMaybes $ map filterCommon xs of
    []  -> Nothing
    xs' -> Just (Node (Both x) xs')
filterCommon t = Just t

treeDifference :: (Eq a) => Tree a -> Tree a -> Forest (Difference a)
treeDifference t1 t2 =
  catMaybes . map filterCommon $ mergeTrees t1 t2

