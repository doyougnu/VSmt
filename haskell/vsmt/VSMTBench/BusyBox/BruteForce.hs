module BruteForce where

import BusyBox
import Utils.VSMTBenchFramework


base :: Integer -> Integer -> Integer
base b = ceiling . logBase (fromInteger b) . fromInteger

hole :: Proposition
hole = bRef "__"


prop :: [Proposition] -> Proposition
prop xs = outer 0 xs
  where
    outer i [x] = x
    outer i xs  = outer (succ i) (inner (T.pack $ show i) xs)


    inner :: T.Text -> [Proposition] -> [Proposition]
    inner _ [] = []
    inner _ [x] = [x]
    inner d (x:y:xs) = ChcB (Dim d) x y : inner d xs

propOpts :: [Proposition] -> Proposition
propOpts = atomize . outer 0
  where
    outer _ [x] = x
    outer i ys  = outer (succ i) (atomize <$> inner (T.pack $ show i) ys)


    inner :: T.Text -> [Proposition] -> [Proposition]
    inner _ [] = []
    inner _ [x] = [x]
    inner d (x:y:ys) = ChcB (Dim d) x y : inner d ys

-- | construct a brute force analysis for an analysis. Check if there is a
-- feature model, if so then prepend it to all the queries
analysisToBF :: Analysis -> [Proposition]
analysisToBF (getAnalysis -> a) = problems
  where
    queries = M.elems a
    problems = case M.lookup FeatureModel a of
                 Nothing -> mconcat queries
                 Just (f:_)  -> concatMap (fmap ((&&&) f)) queries

constructBF :: [Analysis] -> Proposition
constructBF = prop . concatMap analysisToBF
