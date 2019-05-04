module Main where

import Weigh (Grouped (..), Weight (..), Weigh, mainWith, wgroup, func, io, commas, weighResults)
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
  mainWith $ do 
    wgroup "AG" $ do
        func "small" ag ssmall
        func "medium" ag smedium
        func "large" ag slarge
    wgroup "JL" $ do
        func "small" jl ssmall
        func "medium" jl smedium
        func "large" jl slarge
    wgroup "generic-trie" $ do
        func "small" gt ssmall
        func "medium" gt smedium
        func "large" gt slarge
    wgroup "bytestring-trie" $ do
        func "small" bt bssmall
        func "medium" bt bsmedium
        func "large" bt bslarge     
           
