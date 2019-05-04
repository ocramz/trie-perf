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

import Bench.Trie (randTrieInputString, randTrieInputBS, ag, jl, gt, bt)


main :: IO ()
main = withSystemRandom $ \g -> do
  ssmall <- randTrieInputString 3 3 1 g
  smedium <- randTrieInputString 30 30 10 g
  slarge <- randTrieInputString 300 300 100 g
  bssmall <- randTrieInputBS 3 3 1 g
  bsmedium <- randTrieInputBS 30 30 10 g
  bslarge <- randTrieInputBS 300 300 100 g     
  defaultMain [
    bgroup "AG" [
        bench "small" $ whnf ag ssmall
      , bench "medium" $ whnf ag smedium
      , bench "large" $ whnf ag slarge 
              ] ,
    bgroup "JL" [
        bench "small" $ whnf jl ssmall
      , bench "medium" $ whnf jl smedium
      , bench "large" $ whnf ag slarge      
              ] ,
    bgroup "generic-trie" [
        bench "small" $ whnf gt ssmall
      , bench "medium" $ whnf gt smedium
      , bench "large" $ whnf gt slarge      
              ] ,
    bgroup "bytestring-trie" [
        bench "small" $ whnf bt bssmall
      , bench "medium" $ whnf bt bsmedium
      , bench "large" $ whnf bt bslarge      
              ]      
    ]



