{-# language DeriveFunctor #-}
module Data.Trie.BartoszMilewski (Trie, fromList) where

import Data.Fix (Fix(..), cata, ana, hylo)
import GHC.Exts (groupWith)
import Prelude hiding (lookup)
import qualified Prelude as P (lookup)



newtype T2F k a x = T2F [Row k a x] deriving (Show, Functor)
data Row k a x = Row k a x deriving (Show, Functor)
newtype T2 k a = T2 (Fix (T2F k a)) deriving Show
cataT2 :: (T2F k a x -> x) -> T2 k a -> x
cataT2 phi (T2 t) = cata phi t
anaT2 :: (x -> T2F k a x) -> x -> T2 k a
anaT2 psi z = T2 $ ana psi z


newtype TrieF a x = TrieF [(a, x)] deriving (Show, Functor)
newtype Trie a = Trie (Fix (TrieF a)) deriving (Show)


cataT :: (TrieF a x -> x) -> Trie a -> x
cataT phi (Trie t) = cata phi t
anaT :: (x -> TrieF a x) -> x -> Trie a
anaT psi z = Trie $ ana psi z
hyloT :: (TrieF a c -> c) -> (x -> TrieF a x) -> x -> c
hyloT phi psi = cataT phi . anaT psi




-- -- -- lookup :: Ord k => [k] -> Trie k v -> Maybe v
-- lookup ks t = cataT lookupAlg t ks

-- lookupAlg :: Eq a1 => TrieF a1 ([a1] -> [a2]) -> [a1] -> [a2]
-- lookupAlg (TrieF vs) kss = case kss of
--   [] -> []
--   k:ks -> case P.lookup k vs of
--     Nothing -> []
--     Just lf -> lf ks





fromList :: Ord a => [[a]] -> Trie a
fromList = anaT fromListF

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
