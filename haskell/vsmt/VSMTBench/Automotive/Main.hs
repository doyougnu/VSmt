{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Gauge
import           Data.Aeson              (decodeStrict, encodeFile)
import           Data.Either             (lefts, rights)
import qualified Data.ByteString         as BS (readFile, writeFile)
import qualified Data.Text.IO            as T (writeFile, appendFile)
import           Text.Megaparsec         (parse)
import           Data.Maybe              (fromJust)
-- import Control.Monad
import Control.DeepSeq                   (force)

import           Bench.Core
import           Settings
import           Core.Types
import           Core.Core
import           Utils
import           Solve (solveVerbose, solve,solveGetDiag)

import           Lang
import           Auto
import           Parser

autoFileBool :: FilePath
autoFileBool = "VSMTBench/Automotive/Automotive02_merged_evolution_history_boolean.json"

-- | a different file that represents a possible json
smAutoFile :: FilePath
smAutoFile = "VSMTBench/Automotive/vsat_small_example.json"

-- | a chunk of the large autoFile above
chAutoFile :: FilePath
chAutoFile = "VSMTBench/Automotive/vsat_small_chunk.json"

-- main :: IO (V String (Maybe ThmResult))

sliceAndNegate n xs = fromList (&&&) $ bnot <$> drop n xs

ds :: [VariantContext]
ds = toVariantContext . bRef . toDim <$> ["D_0","D_1","D_2","D_3"]
-- D_0 /\    D_2   /\     D_4   /\  D_5
-- <0 /\ <=0 /\ <1 /\ <=1 /\ <2 /\ <= 2

[d0, d2, d4, d5] = ds

-- dimConf' :: VProp Text String String
-- encoding for 6 configs that make sure the inequalities encompass each other
-- sumConf = (d0 &&& fromList (&&&) (bnot <$> tail ds)) -- <0
--           ||| ((bnot d0) &&& d2 &&& (bnot d4 &&& bnot d5))   -- <0 /\ <1
--           ||| ((bnot d0) &&& (bnot d2) &&& d4 &&& bnot d5) -- <0 /\ <1 /\
--           ||| ((bnot d0) &&& (bnot d2) &&& (bnot d4) &&& d5) -- <0 /\ <1 /\

-- | Configs that select only one version
d1Conf :: VariantContext
d1Conf = (d0 &&& fromList (&&&) (bnot <$> tail ds)) -- <0
d2Conf = ((bnot d0) &&& d2 &&& (bnot d4 &&& bnot d5))   -- <0 /\ <1
d3Conf = ((bnot d0) &&& (bnot d2) &&& d4 &&& bnot d5) -- <0 /\ <1 /\
d4Conf = ((bnot d0) &&& (bnot d2) &&& (bnot d4) &&& d5) -- <0 /\ <1 /\

-- | Configs that remove choices and leave that particular choice
justV1Conf :: VariantContext
justV1Conf = (bnot d2) &&& (bnot d4) &&& (bnot d5)
justV2Conf = (bnot d0) &&& (bnot d4) &&& (bnot d5)
justV3Conf = (bnot d0) &&& (bnot d2) &&& (bnot d5)
justV4Conf = (bnot d0) &&& (bnot d2) &&& (bnot d4)

justV12Conf = (bnot d4) &&& (bnot d5)
justV123Conf = (bnot d5)

-- | Compression Ratio setup
-- [pD01Conf, pD12Conf, pD23Conf] = mkCompRatioPairs ds pairs

-- pairs = mkPairs ds

-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  -- readfile is strict
  bJsn <- BS.readFile autoFileBool
  let (Just bAuto) = decodeStrict bJsn :: Maybe Auto
      !bCs = constraints bAuto
      bPs' = parse langParser "" <$> bCs
      failed = lefts bPs'
      !bPs = fmap (simplifyCtxs . renameCtxs sameCtxs) $ rights $ bPs'

      -- | Hardcoding equivalencies in generated dimensions to reduce number of
      -- dimensions to 4
      -- sameDims :: Text -> Text
      -- sameDims d
      --   | d == "D_1" = "D_2"
      --   | d == "D_3" = "D_4"
      --   | otherwise = d

      bProp :: Proposition
      -- !bProp = ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin (bPs)
      !bProp = force $ (naiveEncode . autoToVSat) $ autoAndJoin bPs

  -- Convert the fmf's to actual configurations
  [ppV1]   <- fmap (fromJust . flip validateTotal (getVarFormula d1Conf)) <$> genConfigPool d1Conf
  [ppV2]   <- fmap (fromJust . flip validateTotal (getVarFormula d2Conf)) <$> genConfigPool d2Conf
  [ppV3]   <- fmap (fromJust . flip validateTotal (getVarFormula d3Conf)) <$> genConfigPool d3Conf
  [ppV4]   <- fmap (fromJust . flip validateTotal (getVarFormula d4Conf)) <$> genConfigPool d4Conf

  [justV1] <- genConfigPool justV1Conf
  [justV2] <- genConfigPool justV2Conf
  [justV3] <- genConfigPool justV3Conf
  [justV4] <- genConfigPool justV4Conf

  [justV12] <- genConfigPool justV12Conf
  [justV123] <- genConfigPool justV123Conf

  -- | Compression ratio pairs
  -- [justV12]  <- genConfigPool pD01Conf
  -- [justV23]  <- genConfigPool pD12Conf
  -- [justV34]  <- genConfigPool pD23Conf

  -- confs' <- mkCompRatioConfs ds pairs
  -- let --[v01Conf,v12Conf,v23Conf] = confs'

  -- Plain propositions
  let bPropV1   = fromJust . validatePlain $ configure ppV1 bProp
      bPropV2   = fromJust . validatePlain $ configure ppV2 bProp
      bPropV3   = fromJust . validatePlain $ configure ppV3 bProp
      bPropV4   = fromJust . validatePlain $ configure ppV4 bProp

  -- propositions with a reduced number of choices, these are subsets of
  -- variants
      bPropJustV1 = configure justV1 bProp
      bPropJustV2 = configure justV2 bProp
      bPropJustV3 = configure justV3 bProp
      bPropJustV4 = configure justV4 bProp
      bPropJustV12 = configure justV12 bProp
      bPropJustV123 = configure justV123 bProp

  !v1Variants   <- genVariants bPropJustV1
  !v12Variants  <- genVariants bPropJustV12
  !v123Variants <- genVariants bPropJustV123
  !allVariants  <- genVariants bProp

  let benches :: Settings -> [Benchmark]
      benches ss = [
        -- v - v
        -- mkBench   "v-->v" "V1" d1Conf (solve Nothing ss) (unPlain bPropV1)
        -- , mkBench "v-->v" "V2" d2Conf (solve Nothing ss) (unPlain bPropV2)
        -- , mkBench "v-->v" "V3" d3Conf (solve Nothing ss) (unPlain bPropV3)
        -- , mkBench "v-->v" "V4" d4Conf (solve Nothing ss) (unPlain bPropV4)
        -- , mkBench "v-->v" "V1"           justV1Conf   (solve Nothing ss) bPropJustV1
        -- , mkBench "v-->v" "V1*V2"        justV12Conf  (solve Nothing ss) bPropJustV12
        -- , mkBench "v-->v" "V1*V2*V3"     justV123Conf (solve Nothing ss) bPropJustV123
          mkBench' "v-->v" "V1*V2*V3*V4"  (solve Nothing ss) bProp

       -- p - v
        -- , mkBench'' "p-->v" "V1"  pOnV $ pure bPropV1
        -- , mkBench'' "p-->v" "V2"  pOnV $ pure bPropV2
        -- , mkBench'' "p-->v" "V3"  pOnV $ pure bPropV3
        -- , mkBench'' "p-->v" "V4"  pOnV $ pure bPropV4
        -- , mkBench'' "p-->v" "V1*V2"       pOnV v12Variants
        -- , mkBench'' "p-->v" "V1*V2*V3"    pOnV v123Variants
        -- , mkBench'' "p-->v" "V1*V2*V3*V4" pOnV allVariants

       -- p - p
        -- , mkBench'' "p-->p" "V1"  pOnP $ pure bPropV1
        -- , mkBench'' "p-->p" "V2"  pOnP $ pure bPropV2
        -- , mkBench'' "p-->p" "V3"  pOnP $ pure bPropV3
        -- , mkBench'' "p-->p" "V4"  pOnP $ pure bPropV4
        -- , mkBench'' "p-->p" "V1*V2"        pOnP v12Variants
        -- , mkBench'' "p-->p" "V1*V2*V3"     pOnP v123Variants
        -- , mkBench'' "p-->p" "V1*V2*V3*V4" pOnP allVariants

        -- v - p
        -- , mkBench "v-->p" "V1"  justV1Conf vOnP (unPlain bPropV1)
        -- , mkBench "v-->p" "V2"  justV2Conf vOnP (unPlain bPropV2)
        -- , mkBench "v-->p" "V3"  justV3Conf vOnP (unPlain bPropV3)
        -- , mkBench "v-->p" "V4"  justV4Conf vOnP (unPlain bPropV4)
        -- , mkBench "v-->p" "V1*V2"        justV12Conf vOnP bPropJustV12
        -- , mkBench "v-->p" "V1*V2*V3"     justV123Conf vOnP bPropJustV123
        -- , mkBench' "v-->p" "V1*V2*V3*V4"  vOnP bProp
        ]

  let diagnostics :: FilePath -> Settings -> IO ()
      diagnostics fn ss =
        do runDiagnostics fn "v-->v" "V1"           2  ss bPropJustV1
           runDiagnostics fn "v-->v" "V1*V2"        4  ss bPropJustV12
           runDiagnostics fn "v-->v" "V1*V2*V3"     8  ss bPropJustV123
           runDiagnostics fn "v-->v" "V1*V2*V3*V4"  16 ss bProp

    -- | Compression Ratio props
      -- justbPropV12  = selectVariant v01Conf bProp
      -- justbPropV23  = selectVariant v12Conf bProp
      -- justbPropV34  = selectVariant v23Conf bProp

      -- compRatioBenches :: Settings -> [Benchmark]
      -- compRatioBenches vc =
      --   [
      --     -- v --> v
      --     mkCompBench "v-->v" "V1*V2"  (satWithConf (toDimProp pD01Conf) vc) justbPropV12
      --   , mkCompBench "v-->v" "V2*V3"  (satWithConf (toDimProp pD12Conf) vc) justbPropV23
      --   , mkCompBench "v-->v" "V3*V4"  (satWithConf (toDimProp pD23Conf) vc) justbPropV34

      --     -- v --> p
      --   , mkCompBench "v-->p" "V1*V2"  (vOnPWithConf (toDimProp pD01Conf) vc) justbPropV12
      --   , mkCompBench "v-->p" "V2*V3"  (vOnPWithConf (toDimProp pD12Conf) vc) justbPropV23
      --   , mkCompBench "v-->p" "V3*V4"  (vOnPWithConf (toDimProp pD23Conf) vc) justbPropV34

      --     -- p --> v
      --   , mkCompBench "p-->v" "V1*V2"  (pOnVWithConf (toDimProp pD01Conf) vc) justbPropV12
      --   , mkCompBench "p-->v" "V2*V3"  (pOnVWithConf (toDimProp pD12Conf) vc) justbPropV23
      --   , mkCompBench "p-->v" "V3*V4"  (pOnVWithConf (toDimProp pD23Conf) vc) justbPropV34

      --     -- p --> p
      --   , mkCompBench "p-->p" "V1*V2"  (bfWithConf (toDimProp pD01Conf) vc) justbPropV12
      --   , mkCompBench "p-->p" "V2*V3"  (bfWithConf (toDimProp pD12Conf) vc) justbPropV23
      --   , mkCompBench "p-->p" "V3*V4"  (bfWithConf (toDimProp pD23Conf) vc) justbPropV34
        -- ]

  diagnostics "raw_data/auto_diagnostics_withfix.csv" defSettings

  defaultMain $
    [  bgroup "Z3" (benches defSettings)
    --   bgroup "Z3" (compRatioBenches z3DefConf)
    ]

  let t = bRef "two" ||| bChc "AA" (bRef "a") (bRef "a") -- &&&  bChc "BB" (bRef "c") (bRef "c") -- &&&  bChc "CC" (bRef "c") (bRef "f")&&&  bChc "DD" (bRef "g") (bRef "h")
  -- let t = bRef "one" &&& bRef "one" &&& bChc "AA" (bnot $ bRef "one") (bRef "b") -- ||| bChc "BB" (bRef "c") (bRef "d")
  -- let t = (bChc "AA" (bRef "a") (bRef "b" ||| bRef "c")) ||| bRef "d"
  -- putStrLn $ show $ bProp
  -- let t = bChc "AA" (bRef "Aleft") (bRef "two" ==> bRef "Aright" ||| bRef "one")  -- &&& bRef "two"
  _ <- solveVerbose Nothing defWithModels t
  !res <- solveGetDiag Nothing defWithModels t
  putStrLn $ show res
  -- putStrLn $ show . length $ take 10 $ show res
  -- putStrLn "asddf"
  -- solveForCoreVerbose bProp Nothing
