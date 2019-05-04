module Main where

import Weigh (Grouped (..), Weight (..), Weigh, mainWith, wgroup, func, io, commas, weighResults)
import System.Random.MWC.Probability (withSystemRandom)

import Bench.Trie (randTrieInput, ag, jl, gt)


main :: IO ()
main = withSystemRandom $ \g -> do
  xsmall <- randTrieInput 3 3 1 g
  xmedium <- randTrieInput 30 30 10 g
  xlarge <- randTrieInput 300 300 100 g
  mainWith $ do 
    wgroup "AG" $ do
        func "small" ag xsmall
        func "medium" ag xmedium
        func "large" ag xlarge
    wgroup "JL" $ do
        func "small" jl xsmall
        func "medium" jl xmedium
        func "large" jl xlarge
    wgroup "generic-trie" $ do
        func "small" gt xsmall
        func "medium" gt xmedium
        func "large" gt xlarge        
           
