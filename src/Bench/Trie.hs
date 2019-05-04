module Bench.Trie where

import Control.Monad (replicateM)
import Control.Monad.Primitive
import System.Random.MWC.Probability (create, withSystemRandom, samples, discreteUniform, Prob, Gen, GenIO)

import qualified Data.Trie.AlexanderGreen as AG (Trie, Key, fromList, lookup)
import qualified Data.Trie.JustinLe as JL (Trie, fromList, lookup)

import qualified Data.GenericTrie as GT (Trie, fromList, lookup, TrieKey)


letters :: [Char]
letters = ['a' .. 'z']

randStrings :: PrimMonad m => Int -> Int -> Gen (PrimState m) -> m [String]
randStrings m n g = replicateM m (samples n (discreteUniform letters) g)

nth :: Int -> [c] -> c    
nth n = head . drop (n-1)

randTrieInput :: PrimMonad m =>
                 Int  -- ^ # of random strings
              -> Int  -- ^ length of strings
              -> Int  -- ^ position of key 
              -> Gen (PrimState m)
              -> m ([(String, Int)], String)
randTrieInput m n i g = do
  ss <- randStrings m n g
  let k = nth i ss
  pure (zip ss [0 ..], k)



-- | Data.Trie.AlexanderGreen
ag :: Eq a => ([(AG.Key a, b)], AG.Key a) -> Maybe b
ag (kxs, k) = AG.lookup k $ AG.fromList kxs

-- | Data.Trie.JustinLe
jl :: Ord k => ([([k], v)], [k]) -> Maybe v
jl (kxs, k) = JL.lookup k $ JL.fromList kxs

-- | generic-trie
gt :: GT.TrieKey k => ([(k, a)], k) -> Maybe a
gt (kxs, k) = GT.lookup k $ GT.fromList kxs  
