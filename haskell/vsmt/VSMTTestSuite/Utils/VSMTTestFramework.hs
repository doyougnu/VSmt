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
  , module Data.Core.Types
  , module Data.Core.Result
  , solve
  , goldenVsStringShow
  , goldFile
  ) where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Data.Solve (solve)
import System.FilePath ((</>), (<.>))
import qualified Data.ByteString.Lazy.Char8 as LBC

import Data.Core.Types
import Data.Core.Result

goldFile :: FilePath -> FilePath
goldFile name = "VSMTTestSuite" </> "GoldFiles" </> name <.> "gold"

goldenVsStringShow :: Show a => TestName -> IO a -> TestTree
goldenVsStringShow name result = goldenVsString name (goldFile name) $ LBC.pack . show <$> result
