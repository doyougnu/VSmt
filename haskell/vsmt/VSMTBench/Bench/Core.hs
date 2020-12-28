{-# LANGUAGE BangPatterns #-}

module Bench.Core where

import           Data.List
import           Gauge

import qualified Data.Set   as Set

import           System.IO.Unsafe  (unsafePerformIO)
import           Data.Function (on)
import           Control.DeepSeq (NFData)

import           Core.Core
import           Core.Types
import           Utils


run :: NFData a => String -> (t -> IO a) -> t -> Benchmark
run !desc !f prop = bench desc $! nfIO (f prop)

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

-- | make a description for the benchmark, we input pass through variables alg,
-- and confDesc that are hand written names for the algorithm being used and the
-- configuration/prop description. We then input the prop and get a bunch of
-- statistics on it and return all these as a slash '/' separated string
mkDescription :: String -> String -> [Proposition] -> String
mkDescription alg confDesc []   = error "called mkDescription with no props"
mkDescription alg confDesc [prop] = desc
  where
    !desc' = [ "Chc"        , show nChc
             , "numPlain"   , show nPln
             , "Compression", show ratio
             -- , "VCore_Total", show vCoreTotal
             -- , "VCorePlain" , show vCorePlain
             -- , "VCoreVar"   , show vCoreVar
             , "Variants"   , show variants
             ]
    !desc = mconcat $ intersperse "/" $ pure alg ++ pure confDesc ++ desc'
    !nPln = plainCount prop
    !nChc = choiceCount prop
    ratio :: Float
    !ratio = compressionRatio prop
    -- !(vCoreTotal, vCorePlain, vCoreVar) = unsafePerformIO $ vCoreMetrics prop
    !variants = 2 ^ (Set.size $ dimensions prop)
-- copying code, the greatest of all possible sins. This just isn't important
-- enough to handle properly
mkDescription alg confDesc props = desc
  where
    !desc' = [ "Chc"        , show nChc
             , "numPlain"   , show nPln
             , "Compression", show ratio
             -- , "VCore_Total", show vCoreTotal
             -- , "VCorePlain" , show vCorePlain
             -- , "VCoreVar"   , show vCoreVar
             , "Variants"   , show variants
             ]
    !desc = mconcat $ intersperse "/" $ pure alg ++ pure confDesc ++ desc'
    !nPln = average $ plainCount <$> props
    !nChc = average $ choiceCount <$> props
    ratio :: Float
    !ratio = average $ compressionRatio <$> props
    -- vCoreTotalSum, vCorePlainSum, vCoreVarSum :: Int
    -- !(vCoreTotalSum, vCorePlainSum, vCoreVarSum) =
    --   (foldr (\(x,y,z) (xAcc, yAcc, zAcc) -> (x + xAcc, y + yAcc, z + zAcc)) (0,0,0)
      -- $ (unsafePerformIO . vCoreMetrics) <$> props)
    !variants = average $ (\p -> 2 ^ (Set.size $ dimensions p)) <$> props
    !l = genericLength props
    myDiv = (/) `on` fromIntegral
    -- (vCoreTotal, vCorePlain, vCoreVar) = ( vCoreTotalSum `myDiv` l
    --                                      , vCorePlainSum `myDiv` l
    --                                      , vCoreVarSum `myDiv` l
    --                                      )

-- | Make a benchmark, take two description strings, one to describe the
-- algorithm, one to describe the feature model under analysis, then take a
-- configuration prop, the rest is just pass through parameters to run
-- ex: mkBench "v-->v" "V1"   d0Conf (satWithConf (toDimProp d0Conf) solverConf) bProp
-- ex: mkBench "v-->p" "V1*V2*V3" justD012Conf (bfWith solverConf) justbPropV123
mkBench :: NFData a =>
     String
     -> String
     -> VariantContext
     -> (Proposition -> IO a)
     -> Proposition
     -> Benchmark
mkBench alg confDesc conf !f prop = run desc f prop
  where
    confPool = unsafePerformIO $ genConfigPool conf --just call out to the
                                                      --solver, this should
                                                      --always be safe
    prop' = flip configure prop <$> confPool  -- some confs will never be
                                                  -- total, so we use select
                                                  -- variant here
    desc = mkDescription alg confDesc prop'

-- | like mkBench but we run compression statistics on the prop directly. It is
-- assumed the prop will have been partially selected to reduce it to the
-- compression dimensions of interest
mkCompBench alg confDesc !f prop = run desc f prop
  where
    desc = mkDescription alg confDesc (pure prop)


-- | a version of mkBench that doesn't require the actual configuration. This is
-- used for instances where the proposition under consideration will be solved
-- projected to a plain term many times, such as in the case of running an
-- evolution aware solution. That is, a variational prop will be fully selected
-- to a plain prop which means that the compression ratio statistics will be
-- meaningless because they only make sense with variational terms.
mkBench' :: NFData a =>
            String
     -> String
     -> (Proposition -> IO a)
     -> Proposition
     -> Benchmark
mkBench' alg confDesc !f prop = run desc f prop
  where
    desc = mkDescription alg confDesc (pure prop)

-- | make pairs for controlling complexity for compression ratio benchmark. We
-- want to benchmark two versions that have different compression ratios, but
-- that still run only n solver calls. This way the solver calls do not swamp
-- the compression ratio signal
mkPairs :: [a] -> [[a]]
mkPairs []            = [[]]
mkPairs [_]           = [[]]
mkPairs (x:ys@(y:xs)) = [x,y] : mkPairs ys

-- | Make the compression ratio pair configurations. To Test compression ratio
-- we need to control the number of calls to the solver, so we construct pairs
-- to restrict it to 2 solver calls. Hence if you have 4 features, then we want
-- to test 0-1 1-2 2-3 3-4. The first list should be a list of all dimensions or
-- features, while the second should be a list of pairs
-- mkCompRatioPairs :: [Proposition] -> [[Proposition]] -> [Proposition]
-- mkCompRatioPairs ds = fmap mkPairConf  . filter (not . (<2) . length)
--   where mkPairConf xs@(x:y:_) = (x &&& (negateRest x)) ||| (y &&& (negateRest y))
--           where negateRest a = conjoin $ (bnot <$> (ds \\ pure a))

-- mkCompRatioConfs :: [Proposition] -> [[Proposition]] -> IO [Core.Types.PartialConfig]
-- mkCompRatioConfs ds pairs = mapM (fmap head . genConfigPool . negated) $ filter ((==2) . length) pairs
--   where
--     negated pair = conjoin $ (bnot <$> (ds \\ pair))
