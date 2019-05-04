module Data.Trie.AlexanderGreen (Trie, Key, lookup, fromList) where

import Prelude hiding (lookup)

{- | taken from 
@https://alexandersgreen.wordpress.com/2010/09/13/prefix-trees-in-haskell/@
-}


-- | Prefix tree
data Trie a b = Node (Maybe b) (a -> Maybe (Trie a b)) 

-- | Empty prefix tree
empty :: Trie a b
empty = Node Nothing (const Nothing)

-- | Lookup inside a 'Trie' with a 'Key'
lookup :: Key a -> Trie a b -> Maybe b
lookup [] (Node b _) = b
lookup (x:xs) (Node _ f) = case f x of
  Nothing -> Nothing
  Just pt -> lookup xs pt

-- | A 'Key' is just a synonym for a list
type Key a = [a]

-- | Insert an element at a given key in a 'Trie'
insert :: Eq a => Key a -> b -> Trie a b -> Trie a b
insert [] b (Node _ f) = Node (Just b) f
insert (x:xs) b (Node mb f) =
  case f x of
    Nothing -> Node mb $ insf (insert xs b empty)
    Just pt -> Node mb $ insf (insert xs b pt)
    where
      insf z x' = if x' == x then Just z else f x'

-- | Populate a 'Trie' from a Foldable (eg a list) of ('Key', value) pairs
fromList :: (Foldable t, Eq a) => t (Key a, b) -> Trie a b
fromList = foldl insf empty
  where
    insf t (k, x) = insert k x t
