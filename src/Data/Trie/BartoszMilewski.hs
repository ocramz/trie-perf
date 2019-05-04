{-# language DeriveFunctor #-}
module Data.Trie.BartoszMilewski where

import Data.Fix (Fix(..), cata, cataM, ana, anaM, hylo, hyloM)
import GHC.Exts (groupWith)



newtype TrieF a x = TrieF [(a, x)] deriving (Show, Functor)

newtype Trie a = Trie (Fix (TrieF a)) deriving (Show)


cataT :: (TrieF a x -> x) -> Trie a -> x
cataT phi (Trie t) = cata phi t

anaT :: (x -> TrieF a x) -> x -> Trie a
anaT psi z = Trie $ ana psi z

fromListF :: Ord a => [[a]] -> TrieF a [[a]]
fromListF ss =
  -- are strings empty? (checking one is enough)
  if null (head ss) 
  then TrieF [] -- leaf
  else
    let sss = groupWith head ss
    in TrieF $ fmap mkBranch sss

mkBranch :: [[a]] -> (a, [[a]])
mkBranch sss =
  let c = head (head sss) -- they're all the same
  in (c, fmap tail sss)
