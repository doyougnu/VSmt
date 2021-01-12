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

linuxPaths :: FilePath
linuxPaths = "VSMTBench/Linux/"

-- linux data has 7 dimensions
-- 449017 unique boolean variables
linuxFiles = [ "2016-01-07.json"
             , "2016-01-09.json"
             -- , "2016-01-11.json"
             -- , "2016-01-12.json"
             -- , "2016-01-13.json"
             -- , "2016-01-14.json"
             -- , "2016-01-15.json"
             ]

files = fmap ((++) linuxPaths) linuxFiles

ds :: [VariantContext]
ds = toVariantContext . bRef . toDim <$> ["D_0", "D_1"
                                         -- , "D_2", "D_3", "D_4", "D_5", "D_6"
                                         ]

[  d0
 , d1
 -- , d2
 -- , d3
 -- , d4
 -- , d5
 -- , d6
 ] = ds

mkCascadeConf n xs = conjoin $ (take n xs) ++ (bnot <$> drop n xs)

mkMultConf :: Int -> [VariantContext] -> VariantContext
mkMultConf n xs = conjoin (bnot <$> drop n xs)

justD0Conf         = mkMultConf 1 ds
-- justD01Conf        = mkMultConf 2 ds
-- justD012Conf       = mkMultConf 3 ds
-- justD0123Conf      = mkMultConf 4 ds
-- justD01234Conf     = mkMultConf 5 ds
-- justD012345Conf    = mkMultConf 6 ds


mkConf :: VariantContext -> [VariantContext] -> VariantContext
mkConf x xs = x &&& (conjoin $ bnot <$> (delete x xs))

confs :: [VariantContext]
confs = fmap (flip mkConf ds) ds

d0Conf :: VariantContext
[ d0Conf
  , d1Conf
  -- , d2Conf
  -- , d3Conf
  -- , d4Conf
  -- , d5Conf
  -- , d6Conf
  ] = confs

main = do
  -- readfile is strict
  ls' <- traverse BS.readFile files
  let (Just ls) = (traverse decodeStrict ls') :: Maybe [Auto]
      lConstraints = constraints <$> ls
      lLang = fmap (parse langParser "") <$> lConstraints
      lRight = rights <$> lLang
      lLeft =  lefts <$> lLang

      -- bug of missing variables occurs take 39
      bProp = ((naiveEncode . autoToVSat) . autoAndJoin) $ concat lRight
      -- lProps = ((naiveEncode . autoToVSat) . autoAndJoin) <$> lRight
      -- lProp = ((naiveEncode . autoToVSat) . autoAndJoin) $ (concat lRight)
      -- lProps = ((naiveEncode . autoToVSat) . autoAndJoin) <$> lRight

  [justV1]         <- genConfigPool justD0Conf
  -- [justV12]        <- genConfigPool justD01Conf
  -- [justV123]       <- genConfigPool justD012Conf
  -- [justV1234]      <- genConfigPool justD0123Conf
  -- [justV12345]     <- genConfigPool justD01234Conf
  -- [justV123456]    <- genConfigPool justD012345Conf

  [ppV1]  <- fmap (fromJust . flip validateTotal (getVarFormula d0Conf)) <$> genConfigPool d0Conf
  [ppV2]  <- fmap (fromJust . flip validateTotal (getVarFormula d1Conf)) <$> genConfigPool d1Conf
  -- [ppV3]  <- fmap (fromJust . flip validateTotal (getVarFormula d2Conf)) <$> genConfigPool d2Conf
  -- [ppV4]  <- fmap (fromJust . flip validateTotal (getVarFormula d3Conf)) <$> genConfigPool d3Conf
  -- [ppV5]  <- fmap (fromJust . flip validateTotal (getVarFormula d4Conf)) <$> genConfigPool d4Conf
  -- [ppV6]  <- fmap (fromJust . flip validateTotal (getVarFormula d5Conf)) <$> genConfigPool d5Conf
  -- [ppV7]  <- fmap (fromJust . flip validateTotal (getVarFormula d6Conf)) <$> genConfigPool d6Conf

  -- | choice preserving props
  let !justbPropV1         = configure justV1 bProp
      -- !justbPropV12        = configure justV12 bProp
      -- !justbPropV123       = configure justV123 bProp
      -- !justbPropV1234      = configure justV1234 bProp
      -- !justbPropV12345     = configure justV12345 bProp
      -- !justbPropV123456    = configure justV123456 bProp

    -- | single version props
      !bPropV1  = fromJust . validatePlain $ configure ppV1  bProp
      !bPropV2  = fromJust . validatePlain $ configure ppV2  bProp
      -- !bPropV3  = fromJust . validatePlain $ configure ppV3  bProp
      -- !bPropV4  = fromJust . validatePlain $ configure ppV4  bProp
      -- !bPropV5  = fromJust . validatePlain $ configure ppV5  bProp
      -- !bPropV6  = fromJust . validatePlain $ configure ppV6  bProp
      -- !bPropV7  = fromJust . validatePlain $ configure ppV7  bProp

    -- | sets of variants
  v1Variants         <- genVariants justbPropV1
  -- v12Variants        <- genVariants justbPropV12
  -- v123Variants       <- genVariants justbPropV123
  -- v1234Variants      <- genVariants justbPropV1234
  -- v12345Variants     <- genVariants justbPropV12345
  -- v123456Variants    <- genVariants justbPropV123456
  allVariants        <- genVariants bProp


  let benches :: Settings -> [Benchmark]
      benches solverConf =
        [
          -- v-->v
          -- for the variational props we leave choices in and let the solver find all variants
          mkBench "v-->v" "V1"  d0Conf (solve Nothing solverConf) (unPlain bPropV1)
        , mkBench "v-->v" "V2"  d1Conf (solve Nothing solverConf) (unPlain bPropV2)
        -- , mkBench "v-->v" "V3"  d2Conf (solve Nothing solverConf) (unPlain bPropV3)
        -- , mkBench "v-->v" "V4"  d3Conf (solve Nothing solverConf) (unPlain bPropV4)
        -- , mkBench "v-->v" "V5"  d4Conf (solve Nothing solverConf) (unPlain bPropV5)
        -- , mkBench "v-->v" "V6"  d5Conf (solve Nothing solverConf) (unPlain bPropV6)
        -- , mkBench "v-->v" "V7"  d6Conf (solve Nothing solverConf) (unPlain bPropV7)

        , mkBench "v-->v" "V1"                             justD0Conf         (solve Nothing solverConf) justbPropV1
        -- , mkBench "v-->v" "V1*V2"                          justD01Conf        (solve Nothing solverConf) justbPropV12
        -- , mkBench "v-->v" "V1*V2*V3"                       justD012Conf       (solve Nothing solverConf) justbPropV123
        -- , mkBench "v-->v" "V1*V2*V3*V4"                    justD0123Conf      (solve Nothing solverConf) justbPropV1234
        -- , mkBench "v-->v" "V1*V2*V3*V4*V5"                 justD01234Conf     (solve Nothing solverConf) justbPropV12345
        -- , mkBench "v-->v" "V1*V2*V3*V4*V5*V6"              justD012345Conf    (solve Nothing solverConf) justbPropV123456
        , mkBench' "v-->v" "V1*V2*V3*V4*V5*V6*V7"                             (solve Nothing solverConf) bProp
       -- p - v
        , mkBench'' "p-->v" "V1"  pOnV (pure bPropV1)
        , mkBench'' "p-->v" "V2"  pOnV (pure bPropV2)
        -- , mkBench'' "p-->v" "V3"  pOnV (pure bPropV3)
        -- , mkBench'' "p-->v" "V4"  pOnV (pure bPropV4)
        -- , mkBench'' "p-->v" "V5"  pOnV (pure bPropV5)
        -- , mkBench'' "p-->v" "V6"  pOnV (pure bPropV6)
        -- , mkBench'' "p-->v" "V7"  pOnV (pure bPropV7)
        , mkBench'' "p-->v" "V1"                             pOnV v1Variants
        -- , mkBench'' "p-->v" "V1*V2"                          pOnV v12Variants
        -- , mkBench'' "p-->v" "V1*V2*V3"                       pOnV v123Variants
        -- , mkBench'' "p-->v" "V1*V2*V3*V4"                    pOnV v1234Variants
        -- , mkBench'' "p-->v" "V1*V2*V3*V4*V5"                 pOnV v12345Variants
        -- , mkBench'' "p-->v" "V1*V2*V3*V4*V5*V6"              pOnV v123456Variants
        , mkBench'' "p-->v" "V1*V2*V3*V4*V5*V6*V7"           pOnV allVariants
-- --  -- p - p
        , mkBench'' "p-->p" "V1"  pOnP (pure bPropV1)
        , mkBench'' "p-->p" "V2"  pOnP (pure bPropV2)
        -- , mkBench'' "p-->p" "V3"  pOnP (pure bPropV3)
        -- , mkBench'' "p-->p" "V4"  pOnP (pure bPropV4)
        -- , mkBench'' "p-->p" "V5"  pOnP (pure bPropV5)
        -- , mkBench'' "p-->p" "V6"  pOnP (pure bPropV6)
        -- , mkBench'' "p-->p" "V7"  pOnP (pure bPropV7)
        , mkBench'' "p-->p" "V1"                    pOnP v1Variants
        -- , mkBench'' "p-->p" "V1*V2"                 pOnP v12Variants
        -- , mkBench'' "p-->p" "V1*V2*V3"              pOnP v123Variants
        -- , mkBench'' "p-->p" "V1*V2*V3*V4"           pOnP v1234Variants
        -- , mkBench'' "p-->p" "V1*V2*V3*V4*V5"        pOnP v12345Variants
        -- , mkBench'' "p-->p" "V1*V2*V3*V4*V5*V6"     pOnP v123456Variants
        , mkBench'' "p-->p" "V1*V2*V3*V4*V5*V6*V7"  pOnP allVariants
      -- v - p
        , mkBench "v-->p" "V1"  d0Conf vOnP (unPlain bPropV1)
        , mkBench "v-->p" "V2"  d1Conf vOnP (unPlain bPropV2)
        -- , mkBench "v-->p" "V3"  d2Conf vOnP (unPlain bPropV3)
        -- , mkBench "v-->p" "V4"  d3Conf vOnP (unPlain bPropV4)
        -- , mkBench "v-->p" "V5"  d4Conf vOnP (unPlain bPropV5)
        -- , mkBench "v-->p" "V6"  d5Conf vOnP (unPlain bPropV6)
        -- , mkBench "v-->p" "V7"  d6Conf vOnP (unPlain bPropV7)
        , mkBench "v-->p" "V1"                             justD0Conf         vOnP justbPropV1
        -- , mkBench "v-->p" "V1*V2"                          justD01Conf        vOnP justbPropV12
        -- , mkBench "v-->p" "V1*V2*V3"                       justD012Conf       vOnP justbPropV123
        -- , mkBench "v-->p" "V1*V2*V3*V4"                    justD0123Conf      vOnP justbPropV1234
        -- , mkBench "v-->p" "V1*V2*V3*V4*V5"                 justD01234Conf     vOnP justbPropV12345
        -- , mkBench "v-->p" "V1*V2*V3*V4*V5*V6"              justD012345Conf    vOnP justbPropV123456
        , mkBench' "v-->p" "V1*V2*V3*V4*V5*V6*V7"                             vOnP bProp
        ]

  let diagnostics :: FilePath -> Settings -> IO ()
      diagnostics fn ss =
        do runDiagnostics fn  "v-->v" "V1"                             2    ss justbPropV1
           runDiagnostics fn  "v-->v" "V1*V2"                          4    ss bProp
           -- runDiagnostics fn  "v-->v" "V1*V2"                          4    ss justbPropV12
           -- runDiagnostics fn  "v-->v" "V1*V2*V3"                       8    ss justbPropV123
           -- runDiagnostics fn  "v-->v" "V1*V2*V3*V4"                    16   ss justbPropV1234
           -- runDiagnostics fn  "v-->v" "V1*V2*V3*V4*V5"                 32   ss justbPropV12345
           -- runDiagnostics fn  "v-->v" "V1*V2*V3*V4*V5*V6"              64   ss justbPropV123456
           -- runDiagnostics fn  "v-->v" "V1*V2*V3*V4*V5*V6*V7"           128  ss bProp

  defaultMain $
        [ bgroup "Z3" (benches defSettings)
      -- , bgroup "Z3" (compRatioBenches z3DefConf)
        ]


  -- putStrLn $ show $ fmap length lRight
  -- putStrLn $ show $ fmap length lLeft
