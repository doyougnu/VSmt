-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Core.Parser.Result
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Test cases to ensure the parser on results works
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}

module TestSuite.Core.Parser.Result where

import Parser.Result
import Utils.VSMTTestFramework
import qualified Data.Text.IO as T

import Core.Pretty

------------------------------- Bool Equivalences ------------------------------
tests :: TestTree
tests = testGroup "Result Parser"
  [ testCase "ResultParser fail 1" parserFail
  ]


parserFail :: Assertion
parserFail = do
  let fileName = goldFile "parserTest1"
  result <- liftIO $ solve Nothing defSettings p
  liftIO $! T.writeFile fileName . pretty $ result
  m <- parseFromFile fileName
  assertBool "Didn't parse" $
    case m of
      Left _ -> False
      Right _ -> True
  where p = OpBB Impl (OpIB LTE (iRef "ui") (iRef ("qok" :: Text))) (ChcB "CC" (RefB "l") (RefB "vqn"))
