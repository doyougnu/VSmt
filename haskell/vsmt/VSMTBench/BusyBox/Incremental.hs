module Incremental where

import qualified Data.Map.Strict         as M
import qualified Control.Monad.State.Strict as St

import           Data.Map                (size, Map, toList)
import qualified Data.SBV                as S
import qualified Data.Text               as T

import BusyBox
import Utils.VSMTBenchFramework

eval :: Prop' (S.SBool, a) -> S.SBool
eval (LitB True)     = S.sTrue
eval (LitB False)    = S.sFalse
eval (RefB (b,_))    = b
eval (OpB _ e)       = S.sNot $ eval e
eval (OpBB And l r)  = (S..&&) (eval l) (eval r)
eval (OpBB Or  l r)  = (S..||) (eval l) (eval r)
eval (OpBB Impl l r) = (S..=>) (eval l) (eval r)
eval (OpBB BiImpl l r) = (S..<=>) (eval l) (eval r)
eval (ChcB {}) = error "no choices here!"
eval (OpIB {}) = error "Type Chef throws smt problems?"

  -- S.Symbolic (VProp d (S.SBool, Name) SNum) ->
constructIncremental :: [Analysis] -> IO [[S.SatResult]]
constructIncremental xs = S.runSMT $ do
  let analysisToIncremental (getAnalysis -> a) = Analysis <$> mapM (mapM propToSBool) a

      symbolicAnalyses :: S.Symbolic [Analysis (S.SBool, T.Text)]
      symbolicAnalyses = St.evalStateT (mapM analysisToIncremental xs) (mempty,mempty)

      doAnalysis analysis = do
        let fm            = featureModel analysis
            nM            = noMode analysis
            lexProblems   = lexing analysis
            parseProblems = parsing analysis
            tcProblems    = typeChecking analysis

            runQuery qry  = SC.inNewAssertionStack $ do
              S.constrain $ eval qry
              S.SatResult <$> SC.getSMTResult

        S.constrain (eval fm)
        mapM_ (S.constrain . eval) nM
        lexResults   <- mapM runQuery lexProblems
        parseResults <- mapM runQuery parseProblems
        tcResults    <- mapM runQuery tcProblems
        return $ lexResults <> parseResults <> tcResults

  -- make all variables known to sbv
  s' <- symbolicAnalyses

  -- find the plain stuff and pack the solver
  let plainAnalysis = findPlain s'

  -- constrain the plain stuff
  S.constrain $ eval $ featureModel plainAnalysis
  mapM_ (S.constrain . eval) $ noMode plainAnalysis

  -- off we go
  SC.query $ mapM doAnalysis $ filter (/= plainAnalysis) s'
