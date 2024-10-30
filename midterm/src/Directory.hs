module Directory where

import Tree
import System.Directory (doesDirectoryExist, listDirectory)
import Distribution.FieldGrammar (FilePathNT(FilePathNT))
import qualified Data.Set as Set
import Distribution.Compat.Directory

-- Define an appropriate type called `DirectoryTree` to represent the directory hierarchy
type DirectoryTree = SetTree FilePath

instance Ord DirectoryTree where
    compare :: DirectoryTree -> DirectoryTree -> Ordering
    compare (Leaf a) (Leaf b) = compare a b
    compare (Leaf _) (Node _ _) = LT
    compare (Node _ _) (Leaf _) = GT
    compare (Node a st1) (Node b st2) =
        case compare a a of
            EQ -> compare st1 st2
            other -> other

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    isDir <- doesDirectoryExist path
    if isDir then do
        dirs <- listDirectory path
        files <- mapM buildTree dirs
        return  (Node path (Set.fromList files))
    else
        return (Leaf path)

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree (Leaf a) = a ++ "\n"
defaultDisplayTree (Node a st) = a ++ "\n" ++ concatMap (\s -> ("    " ++ defaultDisplayTree s)) (Set.toList st)

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = undefined