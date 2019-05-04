module Main where

import Control.Monad (replicateM)

import Criterion (Benchmarkable)
import Criterion.Types (Benchmark (..), Report (..), DataRecord( Analysed ), Config (..), SampleAnalysis (..), Verbosity (..), Regression (..))
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Main (defaultMain, bgroup, bench, whnf, whnfIO)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (initializeTime, secs)
import Criterion.Monad (withConfig)

-- import Data.List

import Control.Monad.Primitive
import System.Random.MWC.Probability (create, withSystemRandom, samples, discreteUniform, Prob, Gen, GenIO)

import qualified Data.Trie.AlexanderGreen as AG (Trie, Key, fromList, lookup)
import qualified Data.Trie.JustinLe as JL (Trie, fromList, lookup)



ag :: Eq a => ([(AG.Key a, b)], AG.Key a) -> Maybe b
ag (kxs, k) = AG.lookup k $ AG.fromList kxs

jl :: Ord k => ([([k], v)], [k]) -> Maybe v
jl (kxs, k) = JL.lookup k $ JL.fromList kxs


letters :: [Char]
letters = ['a' .. 'z']

-- randStrings :: Int -> Int -> IO [String] -- 
-- randStrings m n = replicateM m (withSystemRandom io) where
--   io :: GenIO -> IO String
--   io g = samples n (discreteUniform letters) g

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


     

main :: IO ()
main = withSystemRandom $ \g -> defaultMain [
  bgroup "AG" [
      bench "3/3/1" $ whnfIO $ do
          x <- randTrieInput 3 3 1 g
          pure $ ag x
    , bench "30/30/10" $ whnfIO $ do
          x <- randTrieInput 30 30 10 g
          pure $ ag x     
              ]
  ,
  bgroup "JL" [
      bench "3/3/1" $ whnfIO $ do
          x <- randTrieInput 3 3 1 g
          pure $ jl x
    , bench "30/30/10" $ whnfIO $ do
          x <- randTrieInput 30 30 10 g
          pure $ jl x     
              ]  
    
  ]
