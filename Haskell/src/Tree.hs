module Tree where

import Control.Monad
import Data.Maybe
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

data Tree a b = Node a [Tree a b] | Leaf b deriving (Eq, Show, Read)

foldTree :: (a -> [c] -> c) -> (b -> c) -> Tree a b -> c
foldTree  _ fl (Leaf x) = fl x
foldTree fn fl (Node x xs) = fn x $ foldTree fn fl <$> xs

catMaybeTree :: Tree a (Maybe b) -> Maybe (Tree a b)
catMaybeTree = foldTree (\x -> Just . Node x . catMaybes) (fmap Leaf)

filterTree :: (b -> Bool) -> Tree a b -> Maybe (Tree a b)
filterTree p = catMaybeTree . fmap (mfilter p . Just)

instance Bifunctor Tree where
  bimap f s = foldTree (Node . f) (Leaf . s)

instance Bifoldable Tree where
  bifoldMap f = foldTree (\x xs -> f x <> mconcat xs)

instance Bitraversable Tree where
  bitraverse f s =
    foldTree (\x xs -> Node <$> f x <*> sequenceA xs) (fmap Leaf . s)

instance Functor (Tree a) where
  fmap = second

instance Foldable (Tree a) where
  foldMap = bifoldMap mempty

instance Traversable (Tree a) where
  sequenceA = bisequenceA . first pure