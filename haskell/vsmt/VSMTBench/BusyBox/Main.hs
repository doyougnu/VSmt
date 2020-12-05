module Main where

import           Gauge
import qualified Data.Text               as T
import           System.IO

import BusyBox
import Utils.VSMTBenchFramework

dataFile :: FilePath
-- dataFile = "bench/BusyBox/SAT_uniq_sorted.txt"
dataFile = "bench/BusyBox/SAT_problems.txt"

-- | I wonder if having an alternative with all the queries is better than
-- having a list of choices with all the same alternatives...
analysisToVariational :: Analysis T.Text -> Proposition
analysisToVariational a = fm &&& nM &&& lexProblems &&& parseProblems &&& tcProblems
  where fm            = featureModel a
        nM            = conjoin $ noMode a
        lexProblems   = conjoin $ (\p -> bChc "Lexing" p true)       <$> lexing       a
        parseProblems = conjoin $ (\p -> bChc "Parsing" p true)      <$> parsing      a
        tcProblems    = conjoin $ (\p -> bChc "TypeChecking" p true) <$> typeChecking a


constructVariational :: [Analysis T.Text] -> Proposition
constructVariational = conjoin . fmap analysisToVariational

onlyLex = bRef "Lexing" &&& bnot (bRef "Parsing") &&& bnot (bRef "TypeChecking")
onlyParse = bnot (bRef "Lexing") &&& bRef "Parsing" &&& bnot (bRef "TypeChecking")
onlyTypeCheck = bnot (bRef "Lexing") &&& bnot (bRef "Parsing") &&& bRef "TypeChecking"

vc = onlyLex |||
     (onlyLex &&& onlyParse) |||
     (onlyLex &&& onlyParse &&& onlyTypeCheck)


-- run with stack bench --profile vsat:busybox --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  let benches :: Prop' T.Text -> [Analysis T.Text] -> [Benchmark]
      benches solverConf as =
        [ mkBench "BruteForce"  "BusyBox" vc (bfWith solverConf) (constructBF as)
        , mkBench "Variational" "BusyBox" vc (satWith solverConf) (constructVariational as)
        , bench "Incremental/BusyBox" (nfIO (constructIncremental as))
        ]

  ps <- getProblems
  defaultMain
    [ bgroup "Z3" (benches z3DefConf ps)
    ]
  -- satWith z3DefConf (propOpts problems)
  -- dir >>= print
  -- results <- constructIncremental ps
  -- print results


  -- print $ pivotList . prop $ ts
  -- print $ dimensions $ prop ts
  -- print $ prop ts
  -- print $ Set.size $ dimensions $ prop problems
  -- print $ length problems
  -- print $ prop ts
  -- (satWith z3DefConf) bProp >>= encodeFile "data/fin_vmodel.json"
