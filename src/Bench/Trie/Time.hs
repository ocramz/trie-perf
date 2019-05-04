module Bench.Trie.Time where

import Control.Monad (replicateM)
import Control.Monad.Primitive
import System.Random.MWC.Probability (create, withSystemRandom, samples, discreteUniform, Prob, Gen, GenIO)

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
