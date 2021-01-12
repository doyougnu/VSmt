{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson      (decodeStrict, encodeFile)
import           Data.Either     (lefts, rights)
import           Data.List       (delete)
import           Data.Text       (Text,pack)
import           Gauge

import           Control.DeepSeq (force)
import qualified Data.ByteString as BS (readFile)
import           Data.Maybe      (fromJust)
import qualified Data.Text.IO    as T (putStrLn,appendFile, writeFile)
import           Text.Megaparsec (parse)

import           Bench.Core
import           Core.Core
import           Core.Types
import           Core.Pretty
import           Settings
import           Solve           (solve, solveVerbose)
import           Utils

import           Financial
import           Lang
import           Parser

dataFile :: FilePath
dataFile = "VSMTBench/Financial/financial_merged.json"

        -- d == "D_16" = "D_1"
        -- d == "D_12" = "D_17"
        -- d == "D_6" = "D_13"
        -- d == "D_2" = "D_7"
        -- d == "D_10" = "D_3"
        -- d == "D_4" = "D_11"
        -- d == "D_8" = "D_5"
        -- d == "D_14" = "D_9"

sliceAndNegate n xs = fromList (&&&) $ bnot <$> drop n xs

ds :: [VariantContext]
ds = toVariantContext . bRef . toDim <$> ["D_0", "D_16", "D_12", "D_6", "D_2", "D_10", "D_4", "D_8", "D_14", "D_15"]

[d0, d16, d12, d6, d2, d10, d4, d8, d14, d15] = ds

mkCascadeConf n xs = conjoin $ (take n xs) ++ (bnot <$> drop n xs)

mkMultConf :: Int -> [VariantContext] -> VariantContext
mkMultConf n xs = conjoin (bnot <$> drop n xs)

justD0Conf         = mkMultConf 1 ds
justD01Conf        = mkMultConf 2 ds
justD012Conf       = mkMultConf 3 ds
justD0123Conf      = mkMultConf 4 ds
justD01234Conf     = mkMultConf 5 ds
justD012345Conf    = mkMultConf 6 ds
justD0123456Conf   = mkMultConf 7 ds
justD01234567Conf  = mkMultConf 8 ds
justD012345678Conf = mkMultConf 9 ds

-- pairs = mkPairs ds

-- [pD01Conf, pD12Conf, pD23Conf, pD34Conf, pD45Conf, pD56Conf, pD67Conf, pD78Conf, pD89Conf] = mkCompRatioPairs ds pairs

-- ((<,0), = "D_0"})
-- ((≤,0), = "D_1"})
-- ((<,1), = "D_16"})
-- ((≤,1), = "D_17"})
-- ((<,2), = "D_12"})
-- ((≤,2), = "D_13"})
-- ((<,3), = "D_6"})
-- ((≤,3), = "D_7"})
-- ((<,4), = "D_2"})
-- ((≤,4), = "D_3"})
-- ((<,5), = "D_10"})
-- ((≤,5), = "D_11"})
-- ((<,6), = "D_4"})
-- ((≤,6), = "D_5"})
-- ((<,7), = "D_8"})
-- ((≤,7), = "D_9"})
-- ((≤,8), = "D_15"})
-- ((<,8), = "D_14"})
-- dimConf' :: VProp Text String String
-- encoding for 6 configs that make sure the inequalities encompass each other

-- | Hardcoding equivalencies in generated dimensions to reduce number of
-- dimensions to 10, this was manually done by inspecting parser results. There
-- are 18 unique dimensions in the original formula but the inequalities are
-- equal for 8 ranges, thus there are actually only 10 unique dimensions. So we
-- rename them here.
sameDims' :: Text -> Text
sameDims' d
  | d == "D_1"  = "D_16"    -- <= 0 === < 1
  | d == "D_17" = "D_12"    -- <= 1 === < 2
  | d == "D_13" = "D_6"
  | d == "D_7"  = "D_2"
  | d == "D_3"  = "D_10"
  | d == "D_11" = "D_4"
  | d == "D_5"  = "D_8"
  | d == "D_9"  = "D_14"
  | otherwise = d

sameDims :: Dim -> Dim
sameDims = Dim . sameDims' . getDim

mkConf :: VariantContext -> [VariantContext] -> VariantContext
mkConf x xs = x &&& (conjoin $ bnot <$> (delete x xs))

confs :: [VariantContext]
confs = fmap (flip mkConf ds) ds

d0Conf :: VariantContext
[d0Conf, d1Conf, d2Conf, d3Conf, d4Conf, d5Conf, d6Conf, d7Conf, d8Conf, d9Conf] = confs

-- run with stack bench --profile vsat:auto --benchmark-arguments='+RTS -S -RTS --output timings.html'
main = do
  -- readfile is strict
  bJsn <- BS.readFile dataFile
  let (Just bAuto) = decodeStrict bJsn :: Maybe Auto
      !bCs = constraints bAuto

      bPs' = parse langParser "" <$> bCs
      bPs = rights bPs'

      !bProp = force $ ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin (bPs)
      -- dmapping = getDimMap $ autoAndJoin bPs
      -- !bPropOpts = applyOpts defConf bProp

  -- | convert choice preserving vc's to actual confs
  [justV1]         <- genConfigPool justD0Conf
  [justV12]        <- genConfigPool justD01Conf
  [justV123]       <- genConfigPool justD012Conf
  [justV1234]      <- genConfigPool justD0123Conf
  [justV12345]     <- genConfigPool justD01234Conf
  [justV123456]    <- genConfigPool justD012345Conf
  [justV1234567]   <- genConfigPool justD0123456Conf
  [justV12345678]  <- genConfigPool justD01234567Conf
  [justV123456789] <- genConfigPool justD012345678Conf

  -- for each total configuration we have to tell the type system teh config is
  -- total to get the right configure instance
  [ppV1]  <- fmap (fromJust . flip validateTotal (getVarFormula d0Conf)) <$> genConfigPool d0Conf
  [ppV2]  <- fmap (fromJust . flip validateTotal (getVarFormula d1Conf)) <$> genConfigPool d1Conf
  [ppV3]  <- fmap (fromJust . flip validateTotal (getVarFormula d2Conf)) <$> genConfigPool d2Conf
  [ppV4]  <- fmap (fromJust . flip validateTotal (getVarFormula d3Conf)) <$> genConfigPool d3Conf
  [ppV5]  <- fmap (fromJust . flip validateTotal (getVarFormula d4Conf)) <$> genConfigPool d4Conf
  [ppV6]  <- fmap (fromJust . flip validateTotal (getVarFormula d5Conf)) <$> genConfigPool d5Conf
  [ppV7]  <- fmap (fromJust . flip validateTotal (getVarFormula d6Conf)) <$> genConfigPool d6Conf
  [ppV8]  <- fmap (fromJust . flip validateTotal (getVarFormula d7Conf)) <$> genConfigPool d7Conf
  [ppV9]  <- fmap (fromJust . flip validateTotal (getVarFormula d8Conf)) <$> genConfigPool d8Conf
  [ppV10] <- fmap (fromJust . flip validateTotal (getVarFormula d9Conf)) <$> genConfigPool d9Conf

  -- compression ratio pairs
  -- justV12  <- genConfigPool pD01Conf
  -- justV23  <- genConfigPool pD12Conf
  -- justV34  <- genConfigPool pD23Conf
  -- justV45  <- genConfigPool pD34Conf
  -- justV56  <- genConfigPool pD45Conf
  -- justV67  <- genConfigPool pD56Conf
  -- justV78  <- genConfigPool pD67Conf
  -- justV89  <- genConfigPool pD78Conf
  -- justV910 <- genConfigPool pD89Conf

  -- | Compression Ratio Confs to select out other dimensions
  -- [v01Conf,v12Conf,v23Conf,v34Conf,v45Conf,v56Conf,v67Conf,v78Conf,v89Conf] <- mkCompRatioConfs ds pairs

    -- | choice preserving props
  let !justbPropV1         = configure justV1 bProp
      !justbPropV12        = configure justV12 bProp
      !justbPropV123       = configure justV123 bProp
      !justbPropV1234      = configure justV1234 bProp
      !justbPropV12345     = configure justV12345 bProp
      !justbPropV123456    = configure justV123456 bProp
      !justbPropV1234567   = configure justV1234567 bProp
      !justbPropV12345678  = configure justV12345678 bProp
      !justbPropV123456789 = configure justV123456789 bProp

    -- | single version props
      !bPropV1  = fromJust . validatePlain $ configure ppV1  bProp
      !bPropV2  = fromJust . validatePlain $ configure ppV2  bProp
      !bPropV3  = fromJust . validatePlain $ configure ppV3  bProp
      !bPropV4  = fromJust . validatePlain $ configure ppV4  bProp
      !bPropV5  = fromJust . validatePlain $ configure ppV5  bProp
      !bPropV6  = fromJust . validatePlain $ configure ppV6  bProp
      !bPropV7  = fromJust . validatePlain $ configure ppV7  bProp
      !bPropV8  = fromJust . validatePlain $ configure ppV8  bProp
      !bPropV9  = fromJust . validatePlain $ configure ppV9  bProp
      !bPropV10 = fromJust . validatePlain $ configure ppV10 bProp

    -- | sets of variants
  v1Variants         <- genVariants justbPropV1
  v12Variants        <- genVariants justbPropV12
  v123Variants       <- genVariants justbPropV123
  v1234Variants      <- genVariants justbPropV1234
  v12345Variants     <- genVariants justbPropV12345
  v123456Variants    <- genVariants justbPropV123456
  v1234567Variants   <- genVariants justbPropV1234567
  v12345678Variants  <- genVariants justbPropV12345678
  v123456789Variants <- genVariants justbPropV123456789
  allVariants        <- genVariants bProp


  let benches :: Settings -> [Benchmark]
      benches solverConf =
        [
          -- v-->v
          -- for the variational props we leave choices in and let the solver find all variants
          mkBench "v-->v" "V1"  d0Conf (solve Nothing solverConf) (unPlain bPropV1)
        , mkBench "v-->v" "V2"  d1Conf (solve Nothing solverConf) (unPlain bPropV2)
        , mkBench "v-->v" "V3"  d2Conf (solve Nothing solverConf) (unPlain bPropV3)
        , mkBench "v-->v" "V4"  d3Conf (solve Nothing solverConf) (unPlain bPropV4)
        , mkBench "v-->v" "V5"  d4Conf (solve Nothing solverConf) (unPlain bPropV5)
        , mkBench "v-->v" "V6"  d5Conf (solve Nothing solverConf) (unPlain bPropV6)
        , mkBench "v-->v" "V7"  d6Conf (solve Nothing solverConf) (unPlain bPropV7)
        , mkBench "v-->v" "V8"  d7Conf (solve Nothing solverConf) (unPlain bPropV8)
        , mkBench "v-->v" "V9"  d8Conf (solve Nothing solverConf) (unPlain bPropV9)
        , mkBench "v-->v" "V10" d9Conf (solve Nothing solverConf) (unPlain bPropV10)

        , mkBench "v-->v" "V1"                             justD0Conf         (solve Nothing solverConf) justbPropV1
        , mkBench "v-->v" "V1*V2"                          justD01Conf        (solve Nothing solverConf) justbPropV12
        , mkBench "v-->v" "V1*V2*V3"                       justD012Conf       (solve Nothing solverConf) justbPropV123
        , mkBench "v-->v" "V1*V2*V3*V4"                    justD0123Conf      (solve Nothing solverConf) justbPropV1234
        , mkBench "v-->v" "V1*V2*V3*V4*V5"                 justD01234Conf     (solve Nothing solverConf) justbPropV12345
        , mkBench "v-->v" "V1*V2*V3*V4*V5*V6"              justD012345Conf    (solve Nothing solverConf) justbPropV123456
        , mkBench "v-->v" "V1*V2*V3*V4*V5*V6*V7"           justD0123456Conf   (solve Nothing solverConf) justbPropV1234567
        , mkBench "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8"        justD01234567Conf  (solve Nothing solverConf) justbPropV12345678
        , mkBench "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     justD012345678Conf (solve Nothing solverConf) justbPropV123456789
        , mkBench' "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" (solve Nothing solverConf) bProp
       -- p - v
        , mkBench'' "p-->v" "V1"  pOnV (pure bPropV1)
        , mkBench'' "p-->v" "V2"  pOnV (pure bPropV2)
        , mkBench'' "p-->v" "V3"  pOnV (pure bPropV3)
        , mkBench'' "p-->v" "V4"  pOnV (pure bPropV4)
        , mkBench'' "p-->v" "V5"  pOnV (pure bPropV5)
        , mkBench'' "p-->v" "V6"  pOnV (pure bPropV6)
        , mkBench'' "p-->v" "V7"  pOnV (pure bPropV7)
        , mkBench'' "p-->v" "V8"  pOnV (pure bPropV8)
        , mkBench'' "p-->v" "V9"  pOnV (pure bPropV9)
        , mkBench'' "p-->v" "V10" pOnV (pure bPropV10)
        , mkBench'' "p-->v" "V1"                             pOnV v1Variants
        , mkBench'' "p-->v" "V1*V2"                          pOnV v12Variants
        , mkBench'' "p-->v" "V1*V2*V3"                       pOnV v123Variants
        , mkBench'' "p-->v" "V1*V2*V3*V4"                    pOnV v1234Variants
        , mkBench'' "p-->v" "V1*V2*V3*V4*V5"                 pOnV v12345Variants
        , mkBench'' "p-->v" "V1*V2*V3*V4*V5*V6"              pOnV v123456Variants
        , mkBench'' "p-->v" "V1*V2*V3*V4*V5*V6*V7"           pOnV v1234567Variants
        , mkBench'' "p-->v" "V1*V2*V3*V4*V5*V6*V7*V8"        pOnV v12345678Variants
        , mkBench'' "p-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     pOnV v123456789Variants
        , mkBench'' "p-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" pOnV allVariants
-- --  -- p - p
        , mkBench'' "p-->p" "V1"  pOnP (pure bPropV1)
        , mkBench'' "p-->p" "V2"  pOnP (pure bPropV2)
        , mkBench'' "p-->p" "V3"  pOnP (pure bPropV3)
        , mkBench'' "p-->p" "V4"  pOnP (pure bPropV4)
        , mkBench'' "p-->p" "V5"  pOnP (pure bPropV5)
        , mkBench'' "p-->p" "V6"  pOnP (pure bPropV6)
        , mkBench'' "p-->p" "V7"  pOnP (pure bPropV7)
        , mkBench'' "p-->p" "V8"  pOnP (pure bPropV8)
        , mkBench'' "p-->p" "V9"  pOnP (pure bPropV9)
        , mkBench'' "p-->p" "V10" pOnP (pure bPropV10)
        , mkBench'' "p-->p" "V1"                             pOnP v1Variants
        , mkBench'' "p-->p" "V1*V2"                          pOnP v12Variants
        , mkBench'' "p-->p" "V1*V2*V3"                       pOnP v123Variants
        , mkBench'' "p-->p" "V1*V2*V3*V4"                    pOnP v1234Variants
        , mkBench'' "p-->p" "V1*V2*V3*V4*V5"                 pOnP v12345Variants
        , mkBench'' "p-->p" "V1*V2*V3*V4*V5*V6"              pOnP v123456Variants
        , mkBench'' "p-->p" "V1*V2*V3*V4*V5*V6*V7"           pOnP v1234567Variants
        , mkBench'' "p-->p" "V1*V2*V3*V4*V5*V6*V7*V8"        pOnP v12345678Variants
        , mkBench'' "p-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     pOnP v123456789Variants
        , mkBench'' "p-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" pOnP allVariants
      -- v - p
        , mkBench "v-->p" "V1"  d0Conf vOnP (unPlain bPropV1)
        , mkBench "v-->p" "V2"  d1Conf vOnP (unPlain bPropV2)
        , mkBench "v-->p" "V3"  d2Conf vOnP (unPlain bPropV3)
        , mkBench "v-->p" "V4"  d3Conf vOnP (unPlain bPropV4)
        , mkBench "v-->p" "V5"  d4Conf vOnP (unPlain bPropV5)
        , mkBench "v-->p" "V6"  d5Conf vOnP (unPlain bPropV6)
        , mkBench "v-->p" "V7"  d6Conf vOnP (unPlain bPropV7)
        , mkBench "v-->p" "V8"  d7Conf vOnP (unPlain bPropV8)
        , mkBench "v-->p" "V9"  d8Conf vOnP (unPlain bPropV9)
        , mkBench "v-->p" "V10" d9Conf vOnP (unPlain bPropV10)
        , mkBench "v-->p" "V1"                             justD0Conf         vOnP justbPropV1
        , mkBench "v-->p" "V1*V2"                          justD01Conf        vOnP justbPropV12
        , mkBench "v-->p" "V1*V2*V3"                       justD012Conf       vOnP justbPropV123
        , mkBench "v-->p" "V1*V2*V3*V4"                    justD0123Conf      vOnP justbPropV1234
        , mkBench "v-->p" "V1*V2*V3*V4*V5"                 justD01234Conf     vOnP justbPropV12345
        , mkBench "v-->p" "V1*V2*V3*V4*V5*V6"              justD012345Conf    vOnP justbPropV123456
        , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7"           justD0123456Conf   vOnP justbPropV1234567
        , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8"        justD01234567Conf  vOnP justbPropV12345678
        , mkBench "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     justD012345678Conf vOnP justbPropV123456789
        , mkBench' "v-->p" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" vOnP bProp
        ]

  let diagnostics :: FilePath -> Settings -> IO ()
      diagnostics fn ss =
        do runDiagnostics fn  "v-->v" "V1"                             2    ss justbPropV1
           runDiagnostics fn  "v-->v" "V1*V2"                          4    ss justbPropV12
           runDiagnostics fn  "v-->v" "V1*V2*V3"                       8    ss justbPropV123
           runDiagnostics fn  "v-->v" "V1*V2*V3*V4"                    16   ss justbPropV1234
           runDiagnostics fn  "v-->v" "V1*V2*V3*V4*V5"                 32   ss justbPropV12345
           runDiagnostics fn  "v-->v" "V1*V2*V3*V4*V5*V6"              64   ss justbPropV123456
           runDiagnostics fn  "v-->v" "V1*V2*V3*V4*V5*V6*V7"           128  ss justbPropV1234567
           runDiagnostics fn  "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8"        256  ss justbPropV12345678
           runDiagnostics fn  "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9"     512  ss justbPropV123456789
           runDiagnostics fn  "v-->v" "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" 1024 ss bProp

--     -- | Compression Ratio props, we start counting by 1 instead of 0 here
--     justbPropV12'  = selectVariant v01Conf bProp
--     justbPropV23   = selectVariant v12Conf bProp
--     !justbPropV34  = selectVariant v23Conf bProp
--     !justbPropV45  = selectVariant v34Conf bProp
--     !justbPropV56  = selectVariant v45Conf bProp
--     !justbPropV67  = selectVariant v56Conf bProp
--     !justbPropV78  = selectVariant v67Conf bProp
--     !justbPropV89  = selectVariant v78Conf bProp
--     !justbPropV910 = selectVariant v89Conf bProp

--     -- use mkBench' because we are restricting the solver with an fmf not
--     -- through pre-done selection
--     compRatioBenches :: ReadableSMTConf Text -> [Benchmark]
--     compRatioBenches solverConf =
--       [
--         -- v --> v
--         mkCompBench "v-->v" "V1*V2"  (satWithConf (toDimProp pD01Conf) solverConf) justbPropV12'
--       , mkCompBench "v-->v" "V2*V3"  (satWithConf (toDimProp pD12Conf) solverConf) justbPropV23
--       , mkCompBench "v-->v" "V3*V4"  (satWithConf (toDimProp pD23Conf) solverConf) justbPropV34
--       , mkCompBench "v-->v" "V4*V5"  (satWithConf (toDimProp pD34Conf) solverConf) justbPropV45
--       , mkCompBench "v-->v" "V5*V6"  (satWithConf (toDimProp pD45Conf) solverConf) justbPropV56
--       , mkCompBench "v-->v" "V6*V7"  (satWithConf (toDimProp pD56Conf) solverConf) justbPropV67
--       , mkCompBench "v-->v" "V7*V8"  (satWithConf (toDimProp pD67Conf) solverConf) justbPropV78
--       , mkCompBench "v-->v" "V8*V9"  (satWithConf (toDimProp pD78Conf) solverConf) justbPropV89
--       , mkCompBench "v-->v" "V9*V10"  (satWithConf (toDimProp pD89Conf) solverConf) justbPropV910

--         -- p --> v
--       , mkCompBench "p-->v" "V1*V2"  (pOnVWithConf (toDimProp pD01Conf) solverConf) justbPropV12'
--       , mkCompBench "p-->v" "V2*V3"  (pOnVWithConf (toDimProp pD12Conf) solverConf) justbPropV23
--       , mkCompBench "p-->v" "V3*V4"  (pOnVWithConf (toDimProp pD23Conf) solverConf) justbPropV34
--       , mkCompBench "p-->v" "V4*V5"  (pOnVWithConf (toDimProp pD34Conf) solverConf) justbPropV45
--       , mkCompBench "p-->v" "V5*V6"  (pOnVWithConf (toDimProp pD45Conf) solverConf) justbPropV56
--       , mkCompBench "p-->v" "V6*V7"  (pOnVWithConf (toDimProp pD56Conf) solverConf) justbPropV67
--       , mkCompBench "p-->v" "V7*V8"  (pOnVWithConf (toDimProp pD67Conf) solverConf) justbPropV78
--       , mkCompBench "p-->v" "V8*V9"  (pOnVWithConf (toDimProp pD78Conf) solverConf) justbPropV89
--       , mkCompBench "p-->v" "V9*V10" (pOnVWithConf (toDimProp pD89Conf) solverConf) justbPropV910

--         -- p --> p
--       , mkCompBench "p-->p" "V1*V2"  (bfWithConf (toDimProp pD01Conf) solverConf) justbPropV12'
--       , mkCompBench "p-->p" "V2*V3"  (bfWithConf (toDimProp pD12Conf) solverConf) justbPropV23
--       , mkCompBench "p-->p" "V3*V4"  (bfWithConf (toDimProp pD23Conf) solverConf) justbPropV34
--       , mkCompBench "p-->p" "V4*V5"  (bfWithConf (toDimProp pD34Conf) solverConf) justbPropV45
--       , mkCompBench "p-->p" "V5*V6"  (bfWithConf (toDimProp pD45Conf) solverConf) justbPropV56
--       , mkCompBench "p-->p" "V6*V7"  (bfWithConf (toDimProp pD56Conf) solverConf) justbPropV67
--       , mkCompBench "p-->p" "V7*V8"  (bfWithConf (toDimProp pD67Conf) solverConf) justbPropV78
--       , mkCompBench "p-->p" "V8*V9"  (bfWithConf (toDimProp pD78Conf) solverConf) justbPropV89
--       , mkCompBench "p-->p" "V9*V10" (bfWithConf (toDimProp pD89Conf) solverConf) justbPropV910

--         -- p --> v
--       , mkCompBench "v-->p" "V1*V2"  (vOnP (toDimProp pD01Conf) solverConf) justbPropV12'
--       , mkCompBench "v-->p" "V2*V3"  (vOnP (toDimProp pD12Conf) solverConf) justbPropV23
--       , mkCompBench "v-->p" "V3*V4"  (vOnP (toDimProp pD23Conf) solverConf) justbPropV34
--       , mkCompBench "v-->p" "V4*V5"  (vOnP (toDimProp pD34Conf) solverConf) justbPropV45
--       , mkCompBench "v-->p" "V5*V6"  (vOnP (toDimProp pD45Conf) solverConf) justbPropV56
--       , mkCompBench "v-->p" "V6*V7"  (vOnP (toDimProp pD56Conf) solverConf) justbPropV67
--       , mkCompBench "v-->p" "V7*V8"  (vOnP (toDimProp pD67Conf) solverConf) justbPropV78
--       , mkCompBench "v-->p" "V8*V9"  (vOnP (toDimProp pD78Conf) solverConf) justbPropV89
--       , mkCompBench "v-->p" "V9*V10" (vOnP (toDimProp pD89Conf) solverConf) justbPropV910
--       ]

  diagnostics "raw_data/financial_diagnostics.csv" defSettings

  -- defaultMain $
  --       [ bgroup "Z3" (benches defSettings)
  --     -- , bgroup "Z3" (compRatioBenches z3DefConf)
  --       ]
