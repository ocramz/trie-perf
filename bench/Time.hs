module Main where

import Criterion (Benchmarkable)
import Criterion.Types (Benchmark (..), Report (..), DataRecord( Analysed ), Config (..), SampleAnalysis (..), Verbosity (..), Regression (..))
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (initializeTime, secs)
import Criterion.Monad (withConfig)

main = putStrLn "bench time : unimplemented"
