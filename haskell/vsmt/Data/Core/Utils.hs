-----------------------------------------------------------------------------
-- |
-- Module    : Core.Utils
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Core Utilities, we separate from the topmost Utils file because we may want
-- to hide these or not incite a cycle
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror   #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Core.Utils where

import           Control.DeepSeq (NFData)
import           Data.Bifunctor  (Bifunctor (..))
import           Data.Hashable   (Hashable)
import           GHC.Generics    (Generic)
import           Control.Monad.Logger (MonadLogger, logDebug)
import           Prelude hiding (log,putStrLn)

import qualified Data.Text as Text
import           Data.Text.IO (putStrLn)

import Core.Pretty

-- | strict pairs
infix 2 :/\
data a :/\ b = !a :/\ !b
  deriving stock (Eq, Ord,Generic)

instance (NFData a, NFData b) => NFData ((:/\) a b)
instance (Hashable a, Hashable b) => Hashable ((:/\) a b)

sFst :: a :/\ b -> a
{-# INLINE sFst #-}
sFst (a :/\ _) = a

sSnd :: a :/\ b -> b
{-# INLINE sSnd #-}
sSnd (_ :/\ b) = b

instance (Show a, Show b) => Show ((:/\) a b) where
  show (a :/\ b) = show a ++ " :/\\ " ++ show b
instance Functor ((:/\) a) where fmap f (a :/\ b) = a :/\ f b
instance Bifunctor (:/\) where bimap f g (a :/\ b) = f a :/\ g b

log :: MonadLogger m => Text.Text -> m ()
log = $(logDebug)

logWith :: (MonadLogger m, Show a) => Text.Text -> a -> m ()
logWith msg value = log $ msg <> sep <> Text.pack (show value)
  where sep :: Text.Text
        sep = " : "

logIOWith :: Show a => Text.Text -> a -> IO ()
logIOWith msg value = putStrLn $ msg <> sep <> Text.pack (show value)
  where sep :: Text.Text
        sep = " : "

logIO :: Text.Text -> IO ()
logIO = putStrLn

logPretty :: (Pretty a, MonadLogger m, Show a) => Text.Text -> a -> m ()
logPretty msg value = log $ msg <> sep <> pretty value
  where sep :: Text.Text
        sep = " : "

logInThread :: MonadLogger m => Text.Text -> Int -> Text.Text -> m ()
logInThread kind tid msg = log logmsg
  where
    logmsg :: Text.Text
    logmsg = "[" <> kind <> ": " <> Text.pack (show tid) <> "] " <> "==> " <> msg

logInProducer' :: MonadLogger m => Int -> Text.Text -> m ()
logInProducer' = logInThread "Producer"

logInConsumer' :: MonadLogger m => Int -> Text.Text -> m ()
logInConsumer' = logInThread "Consumer"

logInThreadIO :: Text.Text -> Int -> Text.Text -> IO ()
logInThreadIO kind tid msg = putStrLn logmsg
  where
    logmsg :: Text.Text
    logmsg = "[" <> kind <> ": " <> Text.pack (show tid) <> "] " <> "==> " <> msg

logInThreadWith :: (MonadLogger m, Show a) => Text.Text -> Int -> Text.Text -> a -> m ()
logInThreadWith kind tid msg = logWith logmsg
  where
    logmsg :: Text.Text
    logmsg = "[" <> kind <> ": " <> Text.pack (show tid) <> "] " <> "==> " <> msg

logInConsumerWith :: (MonadLogger m, Show a) => Int -> Text.Text -> a -> m ()
logInConsumerWith = logInThreadWith "Consumer"

logInVCWith  :: (MonadLogger m, Show a) => Int -> Text.Text -> a -> m ()
logInVCWith = logInThreadWith "VCWorker"

logInVC' :: MonadLogger m => Int -> Text.Text -> m ()
logInVC' = logInThread "VCWorker"

logInThreadIOWith :: Show a => Text.Text -> Int -> Text.Text -> a -> IO ()
logInThreadIOWith kind tid msg value = putStrLn logmsg
  where logmsg :: Text.Text
        logmsg = "[" <> kind <> ": " <> Text.pack (show tid) <> "] " <> "==> "
                 <> msg <> sep <> Text.pack (show value)

        sep :: Text.Text
        sep = " : "

logInIOProducerWith :: Show a => Int -> Text.Text -> a -> IO ()
logInIOProducerWith = logInThreadIOWith "Producer"

logInIOConsumerWith :: Show a => Int -> Text.Text -> a -> IO ()
logInIOConsumerWith = logInThreadIOWith "Consumer"

logInIOProducer :: Int -> Text.Text -> IO ()
logInIOProducer = logInThreadIO "Producer"

logInIOConsumer :: Int -> Text.Text -> IO ()
logInIOConsumer = logInThreadIO "Consumer"

logInIOVC :: Int -> Text.Text -> IO ()
logInIOVC = logInThreadIO "VCWorker"

logThenPass :: (MonadLogger m, Show b) => Text.Text -> b -> m b
logThenPass str a = logWith str a >> return a

logThreadPass :: (MonadLogger m, Show b) => Int -> Text.Text -> b -> m b
logThreadPass tid str a = logInThreadWith "Thread" tid str a >> return a
