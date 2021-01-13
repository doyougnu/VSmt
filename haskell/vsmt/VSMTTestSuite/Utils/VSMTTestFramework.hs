-----------------------------------------------------------------------------
-- |
-- Module    : Utils.VSMTTestFramework
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Removing redundancies left and right
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

module Utils.VSMTTestFramework
  ( module Test.Tasty
  , module Test.Tasty.Hspec
  , module Test.Tasty.HUnit
  , module Core.Types
  , module Core.Result
  , module Core.Core
  , module Core.Utils
  , module Utils
  , module Utils.VSMTGen
  , goldenVsStringShow
  , goldFile
  , Text
  , liftIO
  , solve
  , solveGetDiag
  , solveVerbose
  , solveForCore
  , defSettings
  , pretty
  , FrozenDiags(..)
  , Counter(..)
  ) where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>), (<.>))
import qualified Data.ByteString.Lazy.Char8 as LBC
import Utils.VSMTGen

import Solve (solve, solveGetDiag, solveVerbose, solveForCore,FrozenDiags(..),Counter(..))
import Settings (defSettings)
import Core.Types
import Core.Result
import Core.Core
import Core.Utils
import Core.Pretty (pretty)
import Utils

goldFile :: FilePath -> FilePath
goldFile name = "VSMTTestSuite" </> "GoldFiles" </> name <.> "gold"

goldenVsStringShow :: Show a => TestName -> IO a -> TestTree
goldenVsStringShow name result = goldenVsString name (goldFile name) $ LBC.pack . show <$> result
