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

-- import Control.Monad.Primitive
import System.Random.MWC.Probability (withSystemRandom)

import Bench.Trie.Time (randTrieInput)

import qualified Data.Trie.AlexanderGreen as AG (Trie, Key, fromList, lookup)
import qualified Data.Trie.JustinLe as JL (Trie, fromList, lookup)

import qualified Data.GenericTrie as GT (Trie, fromList, lookup, TrieKey)



ag :: Eq a => ([(AG.Key a, b)], AG.Key a) -> Maybe b
ag (kxs, k) = AG.lookup k $ AG.fromList kxs

jl :: Ord k => ([([k], v)], [k]) -> Maybe v
jl (kxs, k) = JL.lookup k $ JL.fromList kxs

gt :: GT.TrieKey k => ([(k, a)], k) -> Maybe a
gt (kxs, k) = GT.lookup k $ GT.fromList kxs

main :: IO ()
main = withSystemRandom $ \g -> do
  xsmall <- randTrieInput 3 3 1 g
  xmedium <- randTrieInput 30 30 10 g
  xlarge <- randTrieInput 300 300 100 g   
  defaultMain [
    bgroup "AG" [
        bench "small" $ whnf ag xsmall
      , bench "medium" $ whnf ag xmedium
      , bench "large" $ whnf ag xlarge      
              ] ,
    bgroup "JL" [
        bench "small" $ whnf jl xsmall
      , bench "medium" $ whnf jl xmedium
      , bench "large" $ whnf ag xlarge      
              ] ,
    bgroup "generic-trie" [
        bench "small" $ whnf gt xsmall
      , bench "medium" $ whnf gt xmedium
      , bench "large" $ whnf gt xlarge      
              ]       
    ]



