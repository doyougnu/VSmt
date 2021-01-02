{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Gauge
import           Data.Aeson              (decodeStrict, encodeFile)
import           Data.Either             (lefts, rights)
import qualified Data.ByteString         as BS (readFile)
import qualified Data.Text.IO            as T (writeFile, appendFile)
import           Text.Megaparsec         (parse)
import           Data.Maybe              (fromJust)
import Control.Monad

import           Bench.Core
import           Settings
import           Core.Types
import           Core.Core
import           Utils
import           Solve (solveVerbose, solve)

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
sumConf = (d0 &&& fromList (&&&) (bnot <$> tail ds)) -- <0
          ||| ((bnot d0) &&& d2 &&& (bnot d4 &&& bnot d5))   -- <0 /\ <1
          ||| ((bnot d0) &&& (bnot d2) &&& d4 &&& bnot d5) -- <0 /\ <1 /\
          ||| ((bnot d0) &&& (bnot d2) &&& (bnot d4) &&& d5) -- <0 /\ <1 /\

-- | Configs that select only one version
d0Conf :: VariantContext
d0Conf = (d0 &&& fromList (&&&) (bnot <$> tail ds)) -- <0
d2Conf = ((bnot d0) &&& d2 &&& (bnot d4 &&& bnot d5))   -- <0 /\ <1
d3Conf = ((bnot d0) &&& (bnot d2) &&& d4 &&& bnot d5) -- <0 /\ <1 /\
d4Conf = ((bnot d0) &&& (bnot d2) &&& (bnot d4) &&& d5) -- <0 /\ <1 /\
dAllConf = (d0 &&& d2 &&& d4 &&& d5) -- <0 /\ <1 /\

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
      bPs = fmap (simplifyCtxs . renameCtxs sameCtxs) $ rights $ bPs'

      -- | Hardcoding equivalencies in generated dimensions to reduce number of
      -- dimensions to 4
      -- sameDims :: Text -> Text
      -- sameDims d
      --   | d == "D_1" = "D_2"
      --   | d == "D_3" = "D_4"
      --   | otherwise = d

      bProp :: Proposition
      -- !bProp = ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin (bPs)
      !bProp = (naiveEncode . autoToVSat) $ autoAndJoin bPs

  -- Convert the fmf's to actual configurations
  [ppV1]   <- fmap (fromJust . flip validateTotal (getVarFormula d0Conf)) <$> genConfigPool d0Conf
  [ppV2]   <- fmap (fromJust . flip validateTotal (getVarFormula d2Conf)) <$> genConfigPool d2Conf
  [ppV3]   <- fmap (fromJust . flip validateTotal (getVarFormula d3Conf)) <$> genConfigPool d3Conf
  [ppV4]   <- fmap (fromJust . flip validateTotal (getVarFormula d4Conf)) <$> genConfigPool d4Conf
  [ppVAll] <- fmap (fromJust . flip validateTotal (getVarFormula dAllConf)) <$> genConfigPool dAllConf

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
  let --[v01Conf,v12Conf,v23Conf] = confs'

  let bPropV1   = configure ppV1 bProp
      bPropV2   = configure ppV2 bProp
      bPropV3   = configure ppV3 bProp
      bPropV4   = configure ppV4 bProp
      bPropVAll = configure ppVAll bProp

      bPropJustV1 = configure justV1 bProp
      bPropJustV2 = configure justV2 bProp
      bPropJustV3 = configure justV3 bProp
      bPropJustV4 = configure justV4 bProp
      bPropJustV12 = configure justV12 bProp
      bPropJustV123 = configure justV123 bProp

      benches :: Settings -> [Benchmark]
      benches ss = [
        -- v - v
         mkBench "v-->v" "V1" d0Conf (solve (Just d0Conf) ss) bProp
       , mkBench "v-->v" "V2" d2Conf (solve (Just d2Conf) ss) bProp
       , mkBench "v-->v" "V3" d3Conf (solve (Just d3Conf) ss) bProp
       , mkBench "v-->v" "V4" d4Conf (solve (Just d4Conf) ss) bProp
       , mkBench "v-->v" "V1*V2"        justV12Conf  (solve (Just justV12Conf) ss) bPropJustV12
       , mkBench "v-->v" "V1*V2*V3"     justV123Conf (solve (Just justV123Conf) ss) bPropJustV123
       , mkBench' "v-->v" "V1*V2*V3*V4"  (solve Nothing ss) bProp

       -- p - v
       -- , mkBench "p-->v" "V1"  justV1Conf (pOnV vc) bPropV1
       -- , mkBench "p-->v" "V2"  justV2Conf (pOnV vc) bPropV2
       -- , mkBench "p-->v" "V3"  justV3Conf (pOnV vc) bPropV3
       -- , mkBench "p-->v" "V4"  justV4Conf (pOnV vc) bPropV4
       -- , mkBench "p-->v" "V1*V2"        justV12Conf (pOnV vc) bPropJustV12
       -- , mkBench "p-->v" "V1*V2*V3"     justV123Conf (pOnV vc) bPropJustV123
       -- , mkBench' "p-->v" "V1*V2*V3*V4"  (pOnV vc) bProp

       -- p - p
        -- , mkBench "p-->p" "V1"  justV1Conf (bfWith vc) bPropV1
        -- , mkBench "p-->p" "V2"  justV2Conf (bfWith vc) bPropV2
        -- , mkBench "p-->p" "V3"  justV3Conf (bfWith vc) bPropV3
        -- , mkBench "p-->p" "V4"  justV4Conf (bfWith vc) bPropV4
        -- , mkBench "p-->p" "V1*V2"        justV12Conf (bfWith vc) bPropJustV12
        -- , mkBench "p-->p" "V1*V2*V3"     justV123Conf (bfWith vc) bPropJustV123
        -- , mkBench' "p-->p" "V1*V2*V3*V4"  (bfWith vc) bProp

        -- v - p
        -- , mkBench "v-->p" "V1"  justV1Conf (vOnPWithConf (toDimProp d0Conf) vc) bPropV1
        -- , mkBench "v-->p" "V2"  justV2Conf (vOnPWithConf (toDimProp d2Conf) vc) bPropV2
        -- , mkBench "v-->p" "V3"  justV3Conf (vOnPWithConf (toDimProp d3Conf) vc) bPropV3
        -- , mkBench "v-->p" "V4"  justV4Conf (vOnPWithConf (toDimProp d4Conf) vc) bPropV4
        -- , mkBench "v-->p" "V1*V2"        justV12Conf (vOnPWith vc) bPropJustV12
        -- , mkBench "v-->p" "V1*V2*V3"     justV123Conf (vOnPWith vc) bPropJustV123
        -- , mkBench' "v-->p" "V1*V2*V3*V4"  (vOnPWith vc) bProp
        ]

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

  defaultMain $
    [  bgroup "Z3" (benches defSettings)
    --   bgroup "Z3" (compRatioBenches z3DefConf)
    -- , bgroup "CVC4" (benches cvc4DefConf)
    -- , bgroup "Yices" (benches yicesDefConf)
    -- , bgroup "Boolector" (benches boolectorDefConf)
    ]

  -- let t = bRef "one" &&& bRef "two" &&& bChc "AA" (bRef "a") (bRef "b") &&&  bChc "BB" (bRef "c") (bRef "d") &&&  bChc "CC" (bRef "e") (bRef "f")&&&  bChc "DD" (bRef "g") (bRef "h")
  -- let t = bRef "one" &&& bRef "one" &&& bChc "AA" (bRef "a") (bRef "b") -- ||| bChc "BB" (bRef "c") (bRef "d")
  -- let t = bChc "AA" (bRef "a" ==> bRef "b" &&& bRef "c" &&& bRef "d") true
  -- putStrLn $ show $ bProp
  -- !res <- solve Nothing defSettings t
  -- putStrLn $ show res
  -- putStrLn $ show . length $ take 10 $ show res
  -- putStrLn "asddf"
  -- solveForCoreVerbose bProp Nothing
