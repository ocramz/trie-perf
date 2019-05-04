{-# language PackageImports #-}
module Bench.Trie where

import Control.Monad (replicateM)
import Control.Monad.Primitive
import System.Random.MWC.Probability (create, withSystemRandom, samples, discreteUniform, Prob, Gen, GenIO)

import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B (pack)

import qualified Data.Trie.AlexanderGreen as AG (Trie, Key, fromList, lookup)
import qualified Data.Trie.JustinLe as JL (Trie, fromList, lookup)

import qualified "generic-trie" Data.GenericTrie as GT (Trie, fromList, lookup, TrieKey)
import qualified "bytestring-trie" Data.Trie as BST


letters :: [Char]
letters = ['a' .. 'z']

randStrings :: PrimMonad m =>
               Int -> Int -> Gen (PrimState m) -> m [String]
randStrings m n g = replicateM m (samples n (discreteUniform letters) g)

randBS :: PrimMonad m =>
          Int -> Int -> Gen (PrimState m) -> m [B.ByteString]
randBS m n g = replicateM m io where
  io = do
    str <- samples n (discreteUniform letters) g
    pure $ B.pack str
    

nth :: Int -> [c] -> c    
nth n = head . drop (n-1)

randTrieInput :: (Monad m, Num a, Enum a) =>
                 m [w] -> Int -> m ([(w, a)], w)
randTrieInput io i = do
  ss <- io
  let k = nth i ss
  pure (zip ss [0 ..], k)

randTrieInputString :: PrimMonad m =>
                 Int  -- ^ # of random strings
              -> Int  -- ^ length of strings
              -> Int  -- ^ position of key 
              -> Gen (PrimState m)
              -> m ([(String, Int)], String)
randTrieInputString m n i g = randTrieInput (randStrings m n g) i  

randTrieInputBS :: PrimMonad m =>
                   Int
                -> Int
                -> Int
                -> Gen (PrimState m)
                -> m ([(B.ByteString, Int)], B.ByteString)
randTrieInputBS m n i g = randTrieInput (randBS m n g) i


-- | Data.Trie.AlexanderGreen
ag :: Eq a => ([(AG.Key a, b)], AG.Key a) -> Maybe b
ag (kxs, k) = AG.lookup k $ AG.fromList kxs

-- | Data.Trie.JustinLe
jl :: Ord k => ([([k], v)], [k]) -> Maybe v
jl (kxs, k) = JL.lookup k $ JL.fromList kxs

-- | generic-trie
gt :: GT.TrieKey k => ([(k, a)], k) -> Maybe a
gt (kxs, k) = GT.lookup k $ GT.fromList kxs  

-- | bytestring-trie
bt :: ([(B.ByteString, a)], B.ByteString) -> Maybe a
bt (kxs, k) = BST.lookup k $ BST.fromList kxs  
