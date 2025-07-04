-- Insert and search for numbers in a binary tree.
-- https://exercism.org/tracks/haskell/exercises/binary-search-tree

module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where
data BST a = EmptyBST | BST {
                    node :: a,
                    left :: BST a,
                    right :: BST a} deriving (Eq, Show)
bstLeft :: BST a -> Maybe (BST a)
bstLeft EmptyBST = Nothing
bstLeft (BST a l r) = Just l
bstRight :: BST a -> Maybe (BST a)
bstRight EmptyBST = Nothing
bstRight (BST a l r) = Just r
bstValue :: BST a -> Maybe a
bstValue EmptyBST = Nothing
bstValue (BST a l r) = Just a
empty :: BST a
empty = EmptyBST
fromList :: Ord a => [a] -> BST a
fromList [] = empty
fromList xs = insert (last xs) $ fromList (init xs)
insert :: Ord a => a -> BST a -> BST a
insert x EmptyBST = BST x empty empty
insert x (BST a l r) 
       | x <= a  = BST a (insert x l) r 
       | otherwise = BST a l (insert x r)
singleton :: a -> BST a
singleton x = BST x empty empty
toList :: BST a -> [a]
toList EmptyBST = []
toList (BST a l r) = toList l ++ [a] ++ toList r