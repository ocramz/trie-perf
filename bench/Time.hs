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

import Bench.Trie (randTrieInput, ag, jl, gt)


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



