module Main where

import Criterion (Benchmarkable)
import Criterion.Types (Benchmark (..), Report (..), DataRecord( Analysed ), Config (..), SampleAnalysis (..), Verbosity (..), Regression (..))
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Main (defaultMain, bgroup, bench, whnf)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (initializeTime, secs)
import Criterion.Monad (withConfig)

import Data.List

import qualified Data.Trie.AlexanderGreen as AG (Trie, Key, fromList, lookup)
import qualified Data.Trie.JustinLe as JL (Trie, fromList, lookup)

-- main = putStrLn "bench time : unimplemented"


ag :: (Foldable t, Eq a) => (t (AG.Key a, b), AG.Key a) -> Maybe b
ag (kxs, k) = AG.lookup k $ AG.fromList kxs

jl :: Ord k => ([([k], v)], [k]) -> Maybe v
jl (kxs, k) = JL.lookup k $ JL.fromList kxs


-- wordss n = [w] : w where
--   (w, ws) = splitAt n ['a' .. 'z']

-- wordss n = go [] ['a' .. 'z'] where
--   go acc w = (ws : acc) ++ go wss
--     where (ws, wss) = splitAt n w
    



fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)

main = defaultMain [
  bgroup "AG" [
        bench "1" $ whnf ag ([("mo",1)], "mo")
      , bench "10" $ whnf ag ([("ma",1), ("mb",2), ("mc",1), ("md",2), ("me",1), ("mf",2), ("mg",1), ("mh",2), ("mi",1), ("mj",2)], "mi")
              ],
  bgroup "JL" [
        bench "1" $ whnf jl ([("mo",1)], "mo")
      , bench "10" $ whnf jl ([("ma",1), ("mb",2), ("mc",1), ("md",2), ("me",1), ("mf",2), ("mg",1), ("mh",2), ("mi",1), ("mj",2)], "mi")
              ]    
  ]
