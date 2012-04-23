module Main where

import System.Directory
import System.FilePath
import System.IO.Error
import System.Environment
import Data.List
import Data.Tree
import Data.Function
import Data.Maybe
import Control.Applicative
import Control.Monad

mkDirectoryTree :: FilePath -> IO (Tree FilePath)
mkDirectoryTree path =
  Node (takeFileName path) <$>
         (sort <$> contents path >>= mapM (mkDirectoryTree . ((</>) path)))
  where
    contents path =
      dropDotDirs <$> catchIOError (getDirectoryContents path) (const (return []))
    dropDotDirs = filter (flip notElem $ [".", ".."])

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

directoryDifference d1 d2 = do
  t1 <- filterHidden . fixRootLabel <$> mkDirectoryTree d1
  t2 <- filterHidden . fixRootLabel <$> mkDirectoryTree d2
  return (treeDifference <$> t1  <*> t2)
  where
    fixRootLabel (Node l xs) = Node "root" xs

prettyPrintDirectoryDifference d1 d2 = do
  diff <- directoryDifference d1 d2
  return $ maybe [] (map (drawTree . fmap show)) diff


filterHidden :: Tree FilePath -> Maybe (Tree FilePath)
filterHidden (Node ('.':_) xs) = Nothing
filterHidden (Node x xs) =
  Just $ Node x (catMaybes $ map filterHidden xs)

main = do
  args <- getArgs
  n <- getProgName
  case args of
    ["--help"] -> putStrLn $ "usage: " ++ n ++ " <director1> <directory2>"
    [d1, d2]   -> prettyPrintDirectoryDifference d1 d2 >>= (mapM_ putStrLn)
    otherwise  -> putStrLn "invalid commad (try --help)"
