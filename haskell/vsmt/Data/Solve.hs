-----------------------------------------------------------------------------
-- |
-- Module    : Solve
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module that solves a variational smt problem
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Solve where

import qualified Control.Concurrent.Async              as A (async,cancel,mapConcurrently_)
import qualified Control.Concurrent.STM                as STM (modifyTVar', readTVarIO, newTVarIO, TVar, atomically)
import qualified Control.Concurrent.Chan.Unagi.Bounded as U
import           Control.Monad                         (forever, void, when)
import           Control.Monad.Except                  (MonadError)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Logger                  (LoggingT,
                                                        MonadLogger (..),
                                                        NoLoggingT, logDebug,
                                                        runNoLoggingT,
                                                        runStdoutLoggingT)
import           Control.Monad.Reader                  as R (MonadReader,
                                                             ReaderT (..),
                                                             asks, mapReaderT,
                                                             local,
                                                             runReaderT)
import           Control.Monad.Trans                   (MonadIO, MonadTrans,
                                                        lift)
import qualified Data.HashMap.Strict                   as Map
import           Data.Maybe                            (fromJust)

import qualified Z3.Monad                              as Z
import qualified Data.Text                             as Text
import           GHC.Generics                          (Generic)
import           Control.DeepSeq                       (NFData)

import           Data.Text.IO                          (putStrLn)
import           System.Mem.StableName                 (StableName,makeStableName)
import           Prelude                               hiding (EQ, GT, LT, log,
                                                        putStrLn,read,reads)


import           Core.Pretty
import           Core.Result
import           Core.Types
import           Core.Utils                            ((:/\)(..),sSnd)

import           Settings

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

logInProducer :: (R.MonadReader State m, MonadLogger m) => Text.Text -> m ()
logInProducer msg = R.asks (threadId . channels) >>= flip logInProducer' msg

logInProducerWith :: (R.MonadReader State m, MonadLogger m, Show a) =>
  Text.Text -> a -> m ()
logInProducerWith msg value = do tid <- R.asks (threadId . channels)
                                 logInThreadWith "Producer" tid msg value

logInConsumer :: (R.MonadReader State m, MonadLogger m) => Text.Text -> m ()
logInConsumer msg = R.asks (threadId . channels) >>= flip logInConsumer' msg

logThenPass :: (MonadLogger m, Show b) => Text.Text -> b -> m b
logThenPass str a = logWith str a >> return a

logThreadPass :: (MonadLogger m, Show b) => Int -> Text.Text -> b -> m b
logThreadPass tid str a = logInThreadWith "Thread" tid str a >> return a

------------------------------ Internal Api -------------------------------------
findVCore :: ( MonadLogger m
             , Cachable    m IL (V :/\ IL)
             , Z.MonadZ3   m
             ) => IL -> m VarCore
findVCore = evaluate

solveVerbose :: Maybe VariantContext -> Settings -> Proposition -> IO [(Maybe VariantContext :/\  (Z.Result :/\ [(Var :/\ Value)]))]
solveVerbose = internalSolver runPreSolverLog runSolverLog


solve :: Maybe VariantContext -> Settings -> Prop' Var -> IO [(Maybe VariantContext :/\  (Z.Result :/\ [(Var :/\ Value)]))]
solve = internalSolver runPreSolverNoLog runSolverNoLog

-- TODO fix this horrendous type signature
internalSolver :: (MonadLogger m
                  , MonadLogger f
                  , Cachable f (ExRefType Var) IL'
                  , Cachable f Var IL
                  , Z.MonadZ3 m
                  , Z.MonadZ3 f
                  ) =>
  (Stores -> f IL -> Z.Z3 (IL :/\  Stores))
  -> (State -> SolverT m [(Maybe VariantContext :/\  (Z.Result :/\ [(Var :/\ Value)]))] -> Z.Z3 (b1 :/\  b2))
  -> Maybe VariantContext
  -> Settings
  -> Prop' Var
  -> IO b1
internalSolver preSlvr slvr conf Settings{..} i = do
  (toMain, fromVC)   <- U.newChan vcBufSize
  (toVC,   fromMain) <- U.newChan vcBufSize
  initialStore       <- newStore
  initialCache       <- newCaches
  initialResults     <- newResults


  let il              = toIL i

  -- init the channels
      vcChans   = VCChannels     $ (fromMain, toMain)
      mainChans = MainChannels   $ (fromVC, toVC)
      chans     = Channels{ vcChans=vcChans, mainChans=mainChans, threadId=0}
      startState  = State{ stores   = initialStore
                         , channels = chans
                         , caches   = initialCache
                         , results  = initialResults
                         }
      seasoning = preSlvr initialStore (sSnd <$> il)

      runVCWorkers =
        A.mapConcurrently_ (vcWorker conf startState slvr)
        [1..numVCWorkers]

      -- this thread will exit once it places requests on the producer
      -- chans. If the IL is a unit then it'll be caught by evaluate and
      -- placed on a result chan anyway
      populateChans = Z.evalZ3 $
        do (il' :/\ st) <- seasoning
           slvr startState{stores=st} $
             do void $ findVCore il' >>= removeChoices
                readWith (unResult . results)

  -- kick off
  !aWorkers  <- A.async runVCWorkers

  (results :/\  _) <- populateChans

  A.cancel aWorkers

  return results

solveForCore :: Proposition -> IO (VarCore :/\ State)
solveForCore i = do
  (toMain, fromVC)   <- U.newChan 1
  (toVC,   fromMain) <- U.newChan 1
  initialStore       <- newStore
  initialCache       <- newCaches
  initialResults     <- newResults

  let il              = toIL i

      vcChans   = VCChannels     $ (fromMain, toMain)
      mainChans = MainChannels   $ (fromVC, toVC)
      chans     = Channels{ vcChans=vcChans, mainChans=mainChans, threadId=0}
      startState  = State{ stores   = initialStore
                         , channels = chans
                         , caches   = initialCache
                         , results  = initialResults
                         }
      seasoning = runPreSolverNoLog initialStore (sSnd <$> il)

      populateChans = Z.evalZ3 $
        do (il' :/\ st) <- seasoning
           runSolverNoLog startState{stores=st} $ findVCore il'

  populateChans

------------------------------ Async Helpers -----------------------------------
-- | season the solver, that is prepare it for async workloads
type ThreadID      = Int

-- | A variant context (vc) work is a thread which serves to issue is sat or is not
-- sat calls for the variant context. The producers will issue commands requests
-- on the vc channel and the vc threads will check sat and issue the response.
-- This solver two problems: first it avoids calling allSat at the beginning of
-- the run, such as in VSAT, and secondly it maintains a clear and clean
-- separation between the variant context and the query formula. Or in other
-- words the producer instances don't know about the variant context variables
-- and thus the query formula variables are actually disjoint from dimensions
-- just as we would expect.
vcWorker ::
  (MonadLogger f
  , Z.MonadZ3  f
  , Cachable   f Dim SDimension
  ) => Maybe VariantContext -> State -> (State -> f b -> Z.Z3 a) -> Int -> IO ()
vcWorker Nothing s@State{..} slvr tid =
  let (fromMain, toMain) = getVcChans . vcChans $ channels
  in Z.evalZ3 $ vcHelper fromMain toMain s slvr tid
vcWorker (Just vc) s@State{..} slvr tid =
  Z.evalZ3 $ do
  -- season the solver
  let (fromMain, toMain) = getVcChans . vcChans $ channels
  (b :/\ st) <- runPreSolverLog stores $ contextToSBool vc
  Z.assert (unSDimension b)
  -- now run the helper
  vcHelper fromMain toMain s{stores=st} slvr tid

vcHelper ::
  ( MonadLogger f1
  , Z.MonadZ3 f1
  , Cachable f1 a1 SDimension
  , Functor f2
  ) => U.OutChan (a1, Bool, Maybe SDimension)
  -> U.InChan (Bool, Maybe SDimension)
  -> t
  -> (t -> f1 b -> f2 a2)
  -> Int
  -> f2 ()
vcHelper fromMain toMain st slvr tid =
  -- listen to the channel forever and check requests incrementally against the
  -- context if there is one
    void $
    slvr st $
    forever $
     do
       logInVC' tid "Waiting"
       (d, shouldNegate, vc') <- liftIO $ U.readChan fromMain
       logInVC' tid "got request"
       Z.local $
         do sD <- cached d
            logInVCWith tid "before" sD
            !new <- case vc' of
                      Just e  -> if shouldNegate
                                 then sDNot sD >>= sDAnd e
                                 else sDAnd e sD
                      Nothing -> return sD
            logInVCWith tid "created" new
            Z.assert (unSDimension new)
            Z.check >>= liftIO . \case
              Z.Sat -> U.writeChan toMain (True, Just new)
              _     -> U.writeChan toMain (False, Just new)

------------------------------ Data Types --------------------------------------
-- | Solver configuration is a mapping of dimensions to boolean values, we
-- express this in two ways, first we hold a store of dimensions to symbolic
-- booleans to track what we have seen, secondly we hold a variant context _as_
-- a symbolic formula rather than a data structure so that we can spin up a
-- separate thread to check variant context sat calls when removing choices
type Store = Map.HashMap

newtype SInteger   = SInteger   { unSInteger   :: Z.AST } deriving (Eq,Show)
newtype SDouble    = SDouble    { unSDouble    :: Z.AST } deriving (Eq,Show)
newtype SBool      = SBool      { unSBool      :: Z.AST } deriving (Eq,Show)
newtype SDimension = SDimension { unSDimension :: Z.AST } deriving (Eq,Show)

sBool :: Z.MonadZ3 z3 => Text.Text -> z3 SBool
sBool = fmap SBool . Z.mkFreshBoolVar . Text.unpack

sTrue :: Z.MonadZ3 z3 => z3 SBool
sTrue = SBool <$> Z.mkTrue

sFalse :: Z.MonadZ3 z3 => z3 SBool
sFalse = SBool <$> Z.mkFalse

sDTrue :: Z.MonadZ3 z3 => z3 SDimension
sDTrue = SDimension <$> Z.mkTrue

sDFalse :: Z.MonadZ3 z3 => z3 SDimension
sDFalse = SDimension <$> Z.mkFalse

sDimension :: Z.MonadZ3 z3 => Dim -> z3 SDimension
sDimension = fmap SDimension . Z.mkFreshBoolVar . Text.unpack . getDim

sInteger :: Z.MonadZ3 z3 => Text.Text -> z3 SInteger
sInteger = fmap SInteger . Z.mkFreshIntVar . Text.unpack

sDouble :: Z.MonadZ3 z3 => Text.Text -> z3 SDouble
sDouble = fmap SDouble . Z.mkFreshRealVar . Text.unpack

literalInt :: (Z.MonadZ3 z3, Integral i) => i -> z3 SInteger
literalInt = fmap SInteger . Z.mkIntNum

literalReal :: (Z.MonadZ3 z3, Real r) => r -> z3 SDouble
literalReal = fmap SDouble . Z.mkRealNum

sDNot :: Z.MonadZ3 z3 => SDimension -> z3 SDimension
sDNot = fmap SDimension . Z.mkNot . unSDimension

sNot :: Z.MonadZ3 z3 => SBool -> z3 SBool
sNot = fmap SBool . Z.mkNot . unSBool

sDAnd :: Z.MonadZ3 z3 => SDimension -> SDimension -> z3 SDimension
sDAnd (unSDimension -> l) (unSDimension -> r) = SDimension <$> Z.mkAnd [l,r]

-- Stores of various things
type Ints         = Store Var SInteger
type Doubles      = Store Var SDouble
type Bools        = Store Var SBool
type Dimensions   = Store Dim SDimension
type Context      = Store Dim Bool
type ShouldNegate = Bool

-- channel synonyms for nice types
-- the writer side, the bool value of a sat check
type FromVC = U.OutChan (Bool, SVariantContext)
-- the reader side, a dim, a flag, and the vc
type ToVC   = U.InChan  (Dim, ShouldNegate, SVariantContext)

type FromMain = U.OutChan (Dim, ShouldNegate, SVariantContext)
type ToMain   = U.InChan (Bool, SVariantContext)

newtype VCChannels     = VCChannels     { getVcChans   :: (FromMain, ToMain ) }
newtype MainChannels   = MainChannels   { getMainChans :: (FromVC  , ToVC   ) }

class IxStorable ix where
  type Container ix :: * -> *
  type Container ix = Map.HashMap ix -- the default to hashmap

  add    :: ix -> elem -> Container ix elem -> Container ix elem
  isIn   :: ix -> Container ix elem -> Bool
  find   :: ix -> Container ix elem -> Maybe elem
  adjust :: (elem -> elem) -> ix -> Container ix elem -> Container ix elem

  -- | an unsafe version of find'
  -- TODO: encode the totality of find in the type system
  find'  :: ix -> Container ix elem -> elem
  find' = (fromJust .) . find

instance IxStorable Text.Text where add    = Map.insert
                                    isIn   = Map.member
                                    find   = Map.lookup
                                    adjust = Map.adjust

instance IxStorable Dim where add    = Map.insert
                              isIn   = Map.member
                              find   = Map.lookup
                              adjust = Map.adjust

instance Eq a => IxStorable (StableName a) where
  type Container (StableName a) = Map.HashMap (StableName a)
  add    = Map.insert
  isIn   = Map.member
  find   = Map.lookup
  adjust = Map.adjust

type SVariantContext = Maybe SDimension

-- | The internal state of the solver is just a record that accumulates results
-- and a configuration to track choice decisions. We make a trade off of memory
-- for speed and represent the configuration in several ways. We keep a setlike
-- representation of the config to support setlike operations most notably
-- `member`, we keep a formula representation to send to the result module and
-- we keep the symbolic representation to send to the solver. If we were missing
-- any of these then we would need to translate one to the other which will cost
-- constant time _for every_ choice, hence we want to make that constant factor
-- as small as possible
data State = State
  { stores   :: Stores
  , channels :: Channels
  , caches   :: Caches
  , results  :: Results
  }

type Cache a b = Map.HashMap (StableName a) b
type ACache = Cache IL (V :/\ IL)

newtype Results = Results { unResult :: (STM.TVar [(Maybe VariantContext :/\  (Z.Result :/\ [(Var :/\ Value)]))]) }

deriving instance Generic Z.Result
deriving anyclass instance NFData Z.Result

data Caches = Caches
              { accCache :: STM.TVar ACache
              }

data Stores = Stores
    { vConfig    :: (STM.TVar (Maybe VariantContext))  -- the formula representation of the config
    , sConfig    :: (STM.TVar SVariantContext) -- symbolic representation of a config
    , config     :: (STM.TVar Context)                 -- a map or set representation of the config
    , ints       :: (STM.TVar Ints)
    , doubles    :: (STM.TVar Doubles)
    , bools      :: (STM.TVar Bools)
    , dimensions :: (STM.TVar Dimensions)
    }

data FrozenStores = FrozenStores
    { fvConfig    :: !(Maybe VariantContext)  -- the formula representation of the config
    , fsConfig    :: !SVariantContext -- symbolic representation of a config
    , fconfig     :: !Context                 -- a map or set representation of the config
    , fints       :: !Ints
    , fdoubles    :: !Doubles
    , fbools      :: !Bools
    , fdimensions :: !Dimensions
    }

data Channels = Channels { vcChans   :: VCChannels
                         , mainChans :: MainChannels
                         , threadId  :: !ThreadID
                         }
newCaches :: IO Caches
newCaches = do accCache <- STM.newTVarIO mempty
               return Caches{..}

newResults :: IO Results
newResults = Results <$> STM.newTVarIO mempty

newStore :: IO Stores
newStore = do vConfig    <- STM.newTVarIO mempty
              sConfig    <- STM.newTVarIO Nothing
              config     <- STM.newTVarIO mempty
              ints       <- STM.newTVarIO mempty
              doubles    <- STM.newTVarIO mempty
              bools      <- STM.newTVarIO mempty
              dimensions <- STM.newTVarIO mempty
              return Stores{..}

update :: (R.MonadReader State io, MonadIO io) => (Stores -> STM.TVar a) -> (a -> a) -> io ()
update field = updateWith (field . stores)

reads :: (R.MonadReader State io, MonadIO io) => (Stores -> STM.TVar a) -> io a
reads f = readWith (f . stores)

readCache :: (R.MonadReader State io, MonadIO io) => (Caches -> STM.TVar a) -> io a
readCache f = readWith (f . caches)

updateCache :: (R.MonadReader State io, MonadIO io) => (Caches -> STM.TVar a) -> (a -> a) -> io ()
updateCache field = updateWith (field . caches)

updateWith :: (R.MonadReader s io, MonadIO io) => (s -> STM.TVar a) -> (a -> a) -> io ()
updateWith field f = R.asks field >>=
                     liftIO . STM.atomically . flip STM.modifyTVar' f

readWith :: (R.MonadReader s io, MonadIO io) => (s -> STM.TVar a) -> io a
readWith f = R.asks f  >>= liftIO . STM.readTVarIO

read :: MonadIO io => (s -> STM.TVar a) -> s -> io a
read f = liftIO . STM.readTVarIO . f

freeze :: (R.MonadReader State io, MonadIO io) => io FrozenStores
freeze = do st       <- R.asks stores
            fvConfig <- read vConfig st
            fsConfig <- read sConfig st
            fconfig  <- read config st
            fints    <- read ints st
            fdoubles <- read doubles st
            fbools   <- read bools st
            fdimensions <- read dimensions st
            return FrozenStores{..}

-- | A solver is just a reader over a solver enabled monad. The reader
-- maintains information during the variational execution, such as
-- configuration, variable stores
newtype SolverT m a = SolverT { runSolverT :: R.ReaderT State m a }
  deriving newtype ( Functor,Applicative,Monad,MonadIO
                   , MonadError e, MonadLogger, R.MonadReader State
                   , MonadTrans, Z.MonadZ3
                   )

-- mapSolverT :: (m (a1, Stores) -> m (a2, Stores)) -> SolverT m a1 -> SolverT m a2
mapSolverT :: R.MonadReader r m => (r -> r) -> SolverT m a -> SolverT m a
mapSolverT f = SolverT . R.mapReaderT (local f) . runSolverT

-- | A solver type enabled with query operations and logging
type SolverLog    = SolverT    (LoggingT   Z.Z3)
type Solver       = SolverT    (NoLoggingT Z.Z3)
type PreSolverLog = PreSolverT (LoggingT   Z.Z3)
type PreSolver    = PreSolverT (NoLoggingT Z.Z3)

-- | A presolver runs the first stage of the evaluation/accumulation loop, that
-- is, it is a solver which doesn't understand async, nor incremental push/pops.
-- Rather, it is the solver which generates the first core
newtype PreSolverT m a = PreSolverT { runPreSolverT :: (R.ReaderT Stores m) a }
  deriving newtype ( Functor,Applicative,Monad,MonadIO
                   , MonadError e, MonadTrans, MonadLogger
                   , R.MonadReader Stores , Z.MonadZ3
                   )

runPreSolverLog :: Stores -> PreSolverLog a -> Z.Z3 (a :/\ Stores)
runPreSolverLog s = fmap ( :/\ s) . runStdoutLoggingT . flip R.runReaderT s . runPreSolverT

runPreSolverNoLog :: Stores -> PreSolver a -> Z.Z3 (a :/\ Stores)
runPreSolverNoLog s = fmap ( :/\ s) . runNoLoggingT . flip R.runReaderT s . runPreSolverT

runSolverNoLog :: State -> Solver a -> Z.Z3 (a :/\ State)
runSolverNoLog s = fmap ( :/\ s) . runNoLoggingT . flip R.runReaderT s . runSolverT

runSolverLog :: State -> SolverLog a -> Z.Z3 (a :/\ State)
runSolverLog s = fmap ( :/\ s) . runStdoutLoggingT . flip R.runReaderT s . runSolverT

class RunPreSolver s where
  runPreSolver :: Stores -> s a -> Z.Z3 (a :/\  Stores)

class RunSolver s where
  runSolver :: State -> s a -> Z.Z3 (a :/\  State)

instance Z.MonadZ3 z => Z.MonadZ3 (LoggingT z) where
  getSolver  = lift Z.getSolver
  getContext = lift Z.getContext

instance Z.MonadZ3 z => Z.MonadZ3 (NoLoggingT z) where
  getSolver  = lift Z.getSolver
  getContext = lift Z.getContext


instance RunSolver SolverLog       where runSolver    = runSolverLog
instance RunSolver Solver          where runSolver    = runSolverNoLog
instance RunPreSolver PreSolverLog where runPreSolver = runPreSolverLog
instance RunPreSolver PreSolver    where runPreSolver = runPreSolverNoLog

class Show a => Cachable m a b where cached :: a -> m b

-- -- TODO fix this duplication with derivingVia
instance (Monad m, MonadLogger m, Z.MonadZ3 m) =>
  Cachable (SolverT m) Var IL where
  cached ref = do
    st <- reads bools
    case find ref st of
      Just x -> return (Ref x)
      Nothing -> do
        logInProducerWith "Cache miss on" ref
        newSym <- sBool ref
        update bools (add ref newSym)
        return (Ref newSym)

instance (Z.MonadZ3 m, MonadIO m, MonadLogger m, Monad m) =>
  Cachable (SolverT m) Dim SDimension where
  cached d = do
    st <- reads dimensions
    case find d st of
      Just x -> return x
      Nothing -> do
        newSym <- sDimension d
        update dimensions (add d newSym)
        return newSym

instance (Monad m, MonadIO m, Z.MonadZ3 m) =>
  Cachable (SolverT m) (ExRefType Var) IL' where
  cached (ExRefTypeI i) =
    do st <- reads ints
       case find i st of
         Just x  -> return . Ref' . SI $ x
         Nothing -> do newSym <- sInteger i
                       update ints (add i newSym)
                       return (Ref' . SI $ newSym)

  cached (ExRefTypeD d) =
    do st <- reads doubles
       case find d st of
         Just x  -> return . Ref' $ SD x
         Nothing -> do newSym <- sDouble d
                       update doubles (add d newSym)
                       return $! Ref' $ SD newSym

instance (Monad m, MonadIO m, Z.MonadZ3 m) =>
  Cachable (PreSolverT m) Var IL where
  cached ref   = do st <- readWith bools
                    case find ref st of
                      Just x  -> return (Ref x)
                      Nothing -> do newSym <- sBool ref
                                    updateWith bools (add ref newSym)
                                    return (Ref newSym)


instance (Monad m, MonadIO m, Z.MonadZ3 m) =>
  Cachable (PreSolverT m) (ExRefType Var) IL' where
  cached (ExRefTypeI i) =
    do st <- readWith ints
       case find i st of
         Just x  -> return . Ref' . SI $ x
         Nothing -> do newSym <- sInteger i
                       updateWith ints (add i newSym)
                       return (Ref' . SI $ newSym)

  cached (ExRefTypeD d) =
    do st <- readWith doubles
       case find d st of
         Just x  -> return . Ref' $ SD x
         Nothing -> do newSym <- sDouble d
                       updateWith doubles (add d newSym)
                       return $! Ref' $ SD newSym

instance (Monad m, MonadIO m, Z.MonadZ3 m) =>
  Cachable (PreSolverT m) Dim SDimension where
  cached d = do
    ds <- readWith dimensions
    case find d ds of
      Just x -> return x
      Nothing -> do
        newSym <- sDimension d
        updateWith dimensions (add d newSym)
        return newSym

-- | A general caching mechanism using StableNames. There is a small chance of a
-- collision ~32k per 2^24. I leave this completely unhandled as it is so rare I
-- doubt it'll ever actually occur
instance (Monad m, MonadIO m, MonadLogger m, Z.MonadZ3 m) =>
  Cachable (SolverT m) IL (V :/\ IL) where
  cached !il = do ch <- readCache accCache
                  sn <- liftIO $! il `seq` makeStableName il
                  case find sn ch of
                    Just x  -> logInProducerWith "Acc Cache Hit on" il >> return x
                    Nothing -> do val <- iAccumulate il
                                  val `seq` updateCache accCache (add sn val)
                                  return val

----------------------------------- IL -----------------------------------------
type BRef = SBool

data NRef = SI SInteger
          | SD SDouble
    deriving (Generic,Show,Eq)

-- | The intermediate language, we express negation but negation will not exist
-- in a variational core. The IL language is intermediate and used to collapse
-- the input program to it's essential variational structure. We exploit the
-- symbolic references to represent plain sub-trees such that the only thing
-- that will survive in the variational core are binary connectives, symbolic
-- references and choices
data IL = Unit
    | Ref !BRef
    | BOp   B_B (V :/\ IL)
    | BBOp BB_B (V :/\ IL)  (V :/\ IL)
    | IBOp NN_B (V :/\ IL') (V :/\ IL')
    | Chc Dim Proposition Proposition
    deriving (Generic, Show, Eq)

-- | tags which describes where in the tree there is variation
data V = P | V deriving (Generic, Show, Eq)

-- | property of infection
(<@>) :: V -> V -> V
V <@> V = V
V <@> P = V
P <@> V = V
P <@> P = P


data IL' = Ref' !NRef
    | IOp   N_N (V :/\ IL')
    | IIOp NN_N (V :/\ IL') (V :/\ IL')
    | Chc' Dim NExpression NExpression
    deriving (Generic, Show, Eq)


vCoreSize :: IL -> Int
vCoreSize (BBOp _ (_ :/\ l) (_:/\ r)) = vCoreSize l + vCoreSize r
vCoreSize (BOp _ (_ :/\ e))           = vCoreSize e
vCoreSize (IBOp _ (_:/\ l) (_ :/\ r)) = vCoreSize' l + vCoreSize' r
vCoreSize _            = 1

vCoreSize' :: IL' -> Int
vCoreSize' (IOp _ (_ :/\ e))    = vCoreSize' e
vCoreSize' (IIOp _ (_ :/\ l) (_ :/\ r)) = vCoreSize' l + vCoreSize' r
vCoreSize' _            = 1

vCoreNumPlain :: IL -> Int
vCoreNumPlain Chc {}               = 0
vCoreNumPlain (BOp _ (_ :/\ e))        = vCoreNumPlain e
vCoreNumPlain (BBOp _ (_ :/\ l) (_ :/\ r)) = vCoreNumPlain  l + vCoreNumPlain r
vCoreNumPlain (IBOp _ (_ :/\ l) (_ :/\ r)) = vCoreNumPlain' l + vCoreNumPlain' r
vCoreNumPlain _                    = 1

vCoreNumPlain' :: IL' -> Int
vCoreNumPlain' Chc' {}      = 0
vCoreNumPlain' (IOp _ (_ :/\ e))    = vCoreNumPlain' e
vCoreNumPlain' (IIOp _ (_ :/\ l) (_ :/\ r)) = vCoreNumPlain' l + vCoreNumPlain' r
vCoreNumPlain' _            = 1

vCoreNumVar :: IL -> Int
vCoreNumVar Chc {}       = 0
vCoreNumVar (BOp _ (_ :/\ e))    = vCoreNumVar e
vCoreNumVar (BBOp _ (_ :/\ l) (_ :/\ r)) = vCoreNumVar  l + vCoreNumVar r
vCoreNumVar (IBOp _ (_ :/\ l) (_ :/\ r)) = vCoreNumVar' l + vCoreNumVar' r
vCoreNumVar _            = 1

vCoreNumVar' :: IL' -> Int
vCoreNumVar' Chc' {}      = 0
vCoreNumVar' (IOp _ (_ :/\ e))    = vCoreNumVar' e
vCoreNumVar' (IIOp _ (_ :/\ l) (_ :/\ r)) = vCoreNumVar' l + vCoreNumVar' r
vCoreNumVar'  _           = 1

vCoreMetrics :: Prop' Var -> IO (Int,Int,Int)
vCoreMetrics i = do (core :/\ _) <- solveForCore i
                    let il = getCore core
                        sz  = vCoreSize il
                        nPl = vCoreNumPlain il
                        nVr = vCoreNumVar il
                    return (sz,nPl,nVr)


-- TODO: factor out the redundant cases into a type class
-- | Convert a proposition into the intermediate language to generate a
-- Variational Core
toIL ::
  ( MonadLogger m
  , Cachable    m (ExRefType Var) IL'
  , Cachable    m Var IL
  , Z.MonadZ3   m
  ) => Prop' Var -> m (V :/\ IL)
toIL (LitB True)   = sTrue  >>= return . (P :/\) . Ref
toIL (LitB False)  = sFalse >>= return . (P :/\) . Ref
toIL (RefB ref)    = (P :/\) <$> cached ref
toIL (OpB op e)      = do (v :/\ e') <- toIL e; return (v :/\  BOp op (v :/\ e'))
toIL (OpBB op l r) = do l'@(vl :/\ _) <- toIL l
                        r'@(vr :/\ _) <- toIL r
                        return (vl <@> vr :/\ BBOp op l' r')
toIL (OpIB op l r) = do l'@(vl :/\ _) <- toIL' l
                        r'@(vr :/\ _) <- toIL' r
                        return (vl <@> vr :/\ IBOp op l' r')
toIL (ChcB d l r)  = return (V :/\ Chc d l r)

toIL' :: ( Cachable    m (ExRefType Var) IL'
         , MonadLogger m
         , Z.MonadZ3   m
         ) =>
         NExpr' Var -> m (V :/\ IL')
toIL' (LitI (I i))  = return . (P :/\ ) . Ref' . SI =<< literalInt i
toIL' (LitI (D d))  = return . (P :/\ ) . Ref' . SD =<< literalReal d
toIL' (RefI a)      = (P :/\ ) <$> cached a
toIL' (OpI op e)    = do e'@(v :/\ _) <- toIL' e; return (v :/\  IOp op e')
toIL' (OpII op l r) = do l'@(vl :/\ _) <- toIL' l
                         r'@(vr :/\ _) <- toIL' r
                         return (vl <@> vr :/\ IIOp op l' r')
toIL' (ChcI d l r)  = return (V :/\ Chc' d l r)

-------------------------------- Accumulation -----------------------------------
-- For both evaluation and accumulation we implement the functions in a verbose
-- way to aid the code generator. This is likely not necessary but one missed
-- INLINE could mean a large decrease in performance, because evaluation and
-- accumulation are both extremely hot code we want to make them as fast as
-- possible

-- | A variational core is a partially evaluated AST in the IL language. The
-- goal is to reduce as much as possible all plain terms leaving only symbolic
-- references, choices and logical connectives
newtype VarCore = VarCore {getCore :: IL}
  deriving Show

-- | Helper function to wrap an IL into a variational core
intoCore :: IL -> VarCore
intoCore = VarCore

isUnit :: VarCore -> Bool
isUnit (VarCore Unit) = True
isUnit _              = False

dispatchDOp :: Z.MonadZ3 z3 => BB_B -> SDimension -> SDimension -> z3 SDimension
dispatchDOp And  (unSDimension -> l) (unSDimension -> r) = SDimension <$> Z.mkAnd     [l,r]
dispatchDOp Or   (unSDimension -> l) (unSDimension -> r) = SDimension <$> Z.mkOr      [l,r]
dispatchDOp Impl (unSDimension -> l) (unSDimension -> r) = SDimension <$> Z.mkImplies  l r
dispatchDOp Eqv  (unSDimension -> l) (unSDimension -> r) = SDimension <$> Z.mkEq       l r
dispatchDOp XOr  (unSDimension -> l) (unSDimension -> r) = SDimension <$> Z.mkXor      l r

dispatchOp :: Z.MonadZ3 z3 => BB_B -> SBool -> SBool -> z3 SBool
dispatchOp And  (unSBool -> !l) (unSBool -> !r) = SBool <$> Z.mkAnd     [l,r]
dispatchOp Or   (unSBool -> !l) (unSBool -> !r) = SBool <$> Z.mkOr      [l,r]
dispatchOp Impl (unSBool -> !l) (unSBool -> !r) = SBool <$> Z.mkImplies  l r
dispatchOp Eqv  (unSBool -> !l) (unSBool -> !r) = SBool <$> Z.mkEq       l r
dispatchOp XOr  (unSBool -> !l) (unSBool -> !r) = SBool <$> Z.mkXor      l r

dispatchUOp' :: Z.MonadZ3 z3 => N_N -> NRef -> z3 NRef
dispatchUOp' Neg  (SI i) = SI . SInteger <$> Z.mkUnaryMinus (unSInteger i)
dispatchUOp' Neg  (SD d) = SD . SDouble  <$> Z.mkUnaryMinus (unSDouble  d)
dispatchUOp' Abs  _ = error "absolute value not implemented yet!"
dispatchUOp' Sign _ = error "signum not implemented yet!"

dispatchIOp' :: Z.MonadZ3 z3 => NN_N -> NRef -> NRef -> z3 NRef
dispatchIOp' Add (SI l) (SI r)  = SI . SInteger <$> Z.mkAdd [(unSInteger l),(unSInteger r)]
dispatchIOp' Add (SD l) (SI r)  = SD . SDouble <$> Z.mkAdd  [(unSDouble l) ,(unSInteger r)]
dispatchIOp' Add (SI l) (SD r)  = SD . SDouble <$> Z.mkAdd  [(unSInteger l),(unSDouble r)]
dispatchIOp' Add (SD l) (SD r)  = SD . SDouble <$> Z.mkAdd  [(unSDouble l) ,(unSDouble r)]

dispatchIOp' Mult (SI l) (SI r)  = SI . SInteger <$> Z.mkDiv (unSInteger l) (unSInteger r)
dispatchIOp' Mult (SD l) (SI r)  = SD . SDouble <$> Z.mkDiv  (unSDouble l)  (unSInteger r)
dispatchIOp' Mult (SI l) (SD r)  = SD . SDouble <$> Z.mkDiv  (unSInteger l) (unSDouble r)
dispatchIOp' Mult (SD l) (SD r)  = SD . SDouble <$> Z.mkDiv  (unSDouble l)  (unSDouble r)

dispatchIOp' Sub (SI l) (SI r)  = SI . SInteger <$> Z.mkSub [(unSInteger l),(unSInteger r)]
dispatchIOp' Sub (SD l) (SI r)  = SD . SDouble <$> Z.mkSub  [(unSDouble l) ,(unSInteger r)]
dispatchIOp' Sub (SI l) (SD r)  = SD . SDouble <$> Z.mkSub  [(unSInteger l),(unSDouble r)]
dispatchIOp' Sub (SD l) (SD r)  = SD . SDouble <$> Z.mkSub  [(unSDouble l) ,(unSDouble r)]

dispatchIOp' Div (SI l) (SI r)  = SI . SInteger <$> Z.mkDiv (unSInteger l) (unSInteger r)
dispatchIOp' Div (SD l) (SI r)  = SD . SDouble <$> Z.mkDiv  (unSDouble l)  (unSInteger r)
dispatchIOp' Div (SI l) (SD r)  = SD . SDouble <$> Z.mkDiv  (unSInteger l) (unSDouble r)
dispatchIOp' Div (SD l) (SD r)  = SD . SDouble <$> Z.mkDiv  (unSDouble l)  (unSDouble r)

dispatchIOp' Mod (SI l) (SI r)  = SI . SInteger <$> Z.mkMod (unSInteger l) (unSInteger r)
dispatchIOp' Mod (SD l) (SI r)  = SD . SDouble <$> Z.mkMod  (unSDouble l)  (unSInteger r)
dispatchIOp' Mod (SI l) (SD r)  = SD . SDouble <$> Z.mkMod  (unSInteger l) (unSDouble r)
dispatchIOp' Mod (SD l) (SD r)  = SD . SDouble <$> Z.mkMod  (unSDouble l)  (unSDouble r)

mkEq :: Z.MonadZ3 z3 => Z.AST -> Z.AST -> z3 Z.AST
mkEq x y = do a <- Z.mkGe x y
              b <- Z.mkLe x y
              Z.mkAnd [a,b]

mkNEq :: Z.MonadZ3 z3 => Z.AST -> Z.AST -> z3 Z.AST
mkNEq x y = Z.mkEq x y >>= Z.mkNot

dispatchOp' :: Z.MonadZ3 z3 => NN_B -> NRef -> NRef -> z3 SBool
dispatchOp' LT  (SI l) (SI r) = SBool <$> Z.mkLt (unSInteger l)  (unSInteger r)
dispatchOp' LT  (SD l) (SI r) = SBool <$> Z.mkLt (unSDouble  l)  (unSInteger r)
dispatchOp' LT  (SI l) (SD r) = SBool <$> Z.mkLt (unSInteger l)  (unSDouble  r)
dispatchOp' LT  (SD l) (SD r) = SBool <$> Z.mkLt (unSDouble  l)  (unSDouble  r)

dispatchOp' LTE  (SI l) (SI r) = SBool <$> Z.mkLe (unSInteger l)  (unSInteger r)
dispatchOp' LTE  (SD l) (SI r) = SBool <$> Z.mkLe (unSDouble  l)  (unSInteger r)
dispatchOp' LTE  (SI l) (SD r) = SBool <$> Z.mkLe (unSInteger l)  (unSDouble  r)
dispatchOp' LTE  (SD l) (SD r) = SBool <$> Z.mkLe (unSDouble  l)  (unSDouble  r)

dispatchOp' EQ  (SI l) (SI r) = SBool <$> Z.mkEq (unSInteger l)  (unSInteger r)
dispatchOp' EQ  (SD l) (SI r) = SBool <$> Z.mkEq (unSDouble  l)  (unSInteger r)
dispatchOp' EQ  (SI l) (SD r) = SBool <$> Z.mkEq (unSInteger l)  (unSDouble  r)
dispatchOp' EQ  (SD l) (SD r) = SBool <$> Z.mkEq (unSDouble  l)  (unSDouble  r)

dispatchOp' NEQ (SI l) (SI r) = SBool <$> mkNEq (unSInteger l)  (unSInteger r)
dispatchOp' NEQ (SD l) (SI r) = SBool <$> mkNEq (unSDouble  l)  (unSInteger r)
dispatchOp' NEQ (SI l) (SD r) = SBool <$> mkNEq (unSInteger l)  (unSDouble  r)
dispatchOp' NEQ (SD l) (SD r) = SBool <$> mkNEq (unSDouble  l)  (unSDouble  r)

dispatchOp' GT (SI l) (SI r) = SBool <$> Z.mkGt (unSInteger l)  (unSInteger r)
dispatchOp' GT (SD l) (SI r) = SBool <$> Z.mkGt (unSDouble  l)  (unSInteger r)
dispatchOp' GT (SI l) (SD r) = SBool <$> Z.mkGt (unSInteger l)  (unSDouble  r)
dispatchOp' GT (SD l) (SD r) = SBool <$> Z.mkGt (unSDouble  l)  (unSDouble  r)

dispatchOp' GTE (SI l) (SI r) = SBool <$> Z.mkGe (unSInteger l)  (unSInteger r)
dispatchOp' GTE (SD l) (SI r) = SBool <$> Z.mkGe (unSDouble  l)  (unSInteger r)
dispatchOp' GTE (SI l) (SD r) = SBool <$> Z.mkGe (unSInteger l)  (unSDouble  r)
dispatchOp' GTE (SD l) (SD r) = SBool <$> Z.mkGe (unSDouble  l)  (unSDouble  r)


accumulate :: (Cachable m IL (V :/\ IL), Z.MonadZ3 m) => IL -> m (V :/\ IL)
accumulate = cached

-- | Unmemoized internal Accumulation: we purposefully are verbose to provide
-- the optimizer better opportunities. Accumulation seeks to combine as much as
-- possible the plain terms in the AST into symbolic references
iAccumulate :: Z.MonadZ3 z3 => IL -> z3 (V :/\ IL)
 -- computation rules
iAccumulate Unit    = return (P :/\ Unit)
iAccumulate x@Ref{} = return (P :/\ x)
iAccumulate x@Chc{} = return (V :/\ x)
  -- bools
iAccumulate (BOp Not (_ :/\ Ref r))                 = (P :/\ ) . Ref <$> sNot r
iAccumulate (BBOp op (_ :/\ Ref l) (_ :/\ Ref r))   = (P :/\ ) . Ref <$> dispatchOp op l r
  -- numerics
iAccumulate (IBOp op (_ :/\ Ref' l) (_ :/\ Ref' r)) = (P :/\ ) . Ref <$> dispatchOp' op l r
  -- choices
iAccumulate x@(BBOp _ (_ :/\ Chc {})  (_ :/\ Chc {}))  = return (V :/\ x)
iAccumulate x@(BBOp _ (_ :/\ Ref _)   (_ :/\ Chc {}))  = return (V :/\ x)
iAccumulate x@(BBOp _ (_ :/\ Chc {})  (_ :/\ Ref _))   = return (V :/\ x)
iAccumulate x@(IBOp _ (_ :/\ Chc' {}) (_ :/\ Chc' {})) = return (V :/\ x)
iAccumulate x@(IBOp _ (_ :/\ Ref' _)  (_ :/\ Chc' {})) = return (V :/\ x)
iAccumulate x@(IBOp _ (_ :/\ Chc' {}) (_ :/\ Ref' _))  = return (V :/\ x)
 -- congruence rules
iAccumulate (BOp Not (P :/\ e)) = do (_ :/\ e') <- iAccumulate e
                                     let !res = BOp Not (P :/\ e')
                                     iAccumulate res

iAccumulate (BOp Not (V :/\ e)) = do (_ :/\ e') <- iAccumulate e
                                     let !res = BOp Not (V :/\ e')
                                     return (V :/\ res)

iAccumulate (BBOp op (P :/\ l) (P :/\ r)) = do (_ :/\ l') <- iAccumulate l
                                               (_ :/\ r') <- iAccumulate r
                                               let !res = BBOp op (P :/\ l') (P :/\ r')
                                               iAccumulate res

iAccumulate (BBOp op (_ :/\ l) (_ :/\ r)) = do (vl :/\ l') <- iAccumulate l
                                               (vr :/\ r') <- iAccumulate r
                                               let !res  = BBOp op (vl :/\ l') (vr :/\ r')
                                               return (vl <@> vr :/\ res)

iAccumulate (IBOp op (P :/\ l) (P :/\ r)) = do (_ :/\ l') <- iAccumulate' l
                                               (_ :/\ r') <- iAccumulate' r
                                               let !res = IBOp op (P :/\ l') (P :/\ r')
                                               iAccumulate res

iAccumulate (IBOp op (_ :/\ l) (_ :/\ r)) = do x@(vl :/\ _) <- iAccumulate' l
                                               y@(vr :/\ _) <- iAccumulate' r
                                               let !res = IBOp op x y
                                               return (vl <@> vr :/\  res)

iAccumulate' :: Z.MonadZ3 z3 => IL' -> z3 (V :/\ IL')
  -- computation rules
iAccumulate' x@(Ref' _)                               = return (P :/\  x)
iAccumulate' (IOp op (_ :/\ Ref' n))                  = (P :/\ ) . Ref' <$> dispatchUOp' op n
iAccumulate' (IIOp op (_ :/\ Ref' l) (_ :/\ Ref' r))  = (P :/\) . Ref' <$> dispatchIOp' op l r
  -- choices
iAccumulate' x@Chc' {}                                  = return (V :/\ x)
iAccumulate' x@(IIOp _ (_ :/\ Chc' {}) (_ :/\ Chc' {})) = return (V :/\ x)
iAccumulate' x@(IIOp _ (_ :/\ Ref' _)  (_ :/\ Chc' {})) = return (V :/\ x)
iAccumulate' x@(IIOp _ (_ :/\ Chc' {}) (_ :/\ Ref' _))  = return (V :/\ x)
iAccumulate' (IIOp op c@(_ :/\ Chc'{}) (P :/\ r)) = do !r' <- iAccumulate' r
                                                       return (V :/\ IIOp op c r')
iAccumulate' (IIOp op (P :/\ l) c@(_ :/\ Chc'{})) = do !l' <- iAccumulate' l
                                                       return (V :/\ IIOp op l' c)
  -- congruence rules
iAccumulate' (IIOp o (P :/\ l) (P :/\ r)) = do !x <- iAccumulate' l
                                               !y <- iAccumulate' r
                                               let !res = IIOp o x y
                                               iAccumulate' res

iAccumulate' (IOp o (P :/\  e))  = do !e' <- iAccumulate' e
                                      let !res = IOp o e'
                                      iAccumulate' res

iAccumulate' (IOp o (_ :/\  e))  = do (v :/\ e') <- iAccumulate' e
                                      let res = IOp o (v :/\ e')
                                      return (v :/\ res)

iAccumulate' (IIOp o (_ :/\ l) (_ :/\ r)) = do x@(vl :/\ _) <- iAccumulate' l
                                               y@(vr :/\ _) <- iAccumulate' r
                                               let !res = IIOp o x y
                                               return (vl <@> vr :/\  res)

-------------------------------- Evaluation -----------------------------------
toSolver :: (Monad m, Z.MonadZ3 m) => SBool -> m VarCore
-- {-# INLINE toSolver #-}
toSolver (unSBool -> a) = do Z.assert a; return $! intoCore Unit

isValue :: IL -> Bool
isValue Unit    = True
isValue (Ref _) = True
isValue _       = False

isValue' :: IL' -> Bool
isValue' (Ref' _) = True
isValue' _        = False

-- | Evaluation will remove plain terms when legal to do so, "sending" these
-- terms to the solver, replacing them to Unit to reduce the size of the
-- variational core
  -- computation rules
evaluate :: ( MonadLogger m
            , Z.MonadZ3   m
            , Cachable    m IL (V :/\ IL)
            ) => IL -> m VarCore
evaluate Unit     = return $! intoCore Unit
evaluate (Ref b)  = toSolver b
evaluate x@Chc {} = return $! intoCore x
  -- bools
evaluate (BOp Not (_ :/\ Ref r))                  = sNot r >>= toSolver
evaluate (BBOp op (_ :/\ Ref l) (_ :/\ Ref r))    = toSolver =<< dispatchOp op l r
  -- numerics
evaluate (IBOp op  (_ :/\ Ref' l) (_ :/\ Ref' r)) = toSolver =<< dispatchOp' op l r
  -- choices
evaluate x@(BBOp _ (V :/\ _) (V :/\ _)) = return $! intoCore x
evaluate x@(IBOp _ (V :/\ _) (V :/\ _)) = return $! intoCore x
  -- congruence cases
evaluate (BBOp And (_ :/\ l) (_ :/\ Unit))      = evaluate l
evaluate (BBOp And (_ :/\ Unit) (_ :/\ r))      = evaluate r
evaluate (BBOp And (_ :/\ l) (_ :/\ x@(Ref _))) = do _ <- evaluate x; evaluate l
evaluate (BBOp And (_ :/\ x@(Ref _)) (_ :/\ r)) = do _ <- evaluate x; evaluate r
evaluate (IBOp op (P :/\ l) (P :/\ r))          = do !l' <- iAccumulate' l
                                                     !r' <- iAccumulate' r
                                                     let !res = IBOp op l' r'
                                                     evaluate res

evaluate (IBOp op (_ :/\ l) (_ :/\ r))          = do l' <- iAccumulate' l
                                                     r' <- iAccumulate' r
                                                     let res = IBOp op l' r'
                                                     return $! intoCore res

  -- accumulation cases
evaluate x@(BOp Not (P :/\ _))  = accumulate x >>= evaluate . sSnd
evaluate x@(BOp Not (V :/\ _))  = intoCore . sSnd <$> accumulate x
evaluate (BBOp And (P :/\ l) (P :/\ r)) = log "[Eval P P] And case" >>
  do (VarCore l') <- evaluate l
     (VarCore r') <- evaluate r
     let !res = BBOp And (P :/\ l') (P :/\ r')
     evaluate res
evaluate (BBOp And (V :/\ l) (P :/\ r)) = log "[Eval V P] And case" >>
  do (VarCore r') <- evaluate r
     let !res = BBOp And (V :/\ l) (P :/\ r')
     evaluate res
evaluate (BBOp And (P :/\ l) (V :/\ r)) = log "[Eval P V] And case" >>
  do (VarCore l') <- evaluate l
     let !res = BBOp And (P :/\ l') (V :/\ r)
     evaluate res
evaluate (BBOp op (P :/\ l) (P :/\ r)) = log "[Eval P P] General Case" >>
  do (_ :/\ l') <- accumulate l
     (_ :/\ r') <- accumulate r
     let !res = BBOp op (P :/\ l') (P :/\ r')
     evaluate res
evaluate (BBOp op (V :/\ l) (P :/\ r)) = log "[Eval V P] General Case" >>
  do (_ :/\ r') <- accumulate r
     let !res = BBOp op (V :/\ l) (P :/\ r')
     return $! intoCore res
evaluate (BBOp op (P :/\ l) (V :/\ r)) = log "[Eval P V] General Case" >>
  do (_ :/\ l') <- accumulate l
     let !res = BBOp op (P :/\ l') (V :/\ r)
     return $! intoCore res

------------------------- Removing Choices -------------------------------------
-- TODO transform to a GADT
-- | We use a zipper to track the context when searching for choices, this
-- removes the need to perform tree rotations. We make this as strict as
-- possible because we know we will be consuming the entire structure so there
-- is not need to build thunks
data Ctx = InL  !Ctx   BB_B  IL
         | InR  !SBool BB_B  !Ctx
         | InLB !Ctx   NN_B  IL'
         | InRB NRef   NN_B  !Ctx
         | InL' !Ctx   NN_N  IL'
         | InR' NRef   NN_N  !Ctx
         | InU         B_B   !Ctx
         | InU'        N_N   !Ctx
         | Top
    deriving Show

data Loc = InBool !IL !Ctx
         | InNum !IL' !Ctx
         deriving Show

toLoc :: IL -> Loc
toLoc = toLocWith Top

toLoc' :: IL' -> Loc
toLoc' = toLocWith' Top

toLocWith :: Ctx -> IL -> Loc
toLocWith = flip InBool

toLocWith' :: Ctx -> IL' -> Loc
toLocWith' = flip InNum

findChoice :: ( Z.MonadZ3 z3
              , Cachable  z3 IL (V :/\ IL)
              ) => Loc -> z3 Loc
  -- base cases
findChoice x@(InBool Ref{} Top) = return x
findChoice x@(InBool Unit Top)  = return x
findChoice x@(InBool Chc {} _)  = return x
findChoice x@(InNum Chc' {} _)  = return x
  -- discharge two references
findChoice (InBool l@Ref {} (InL parent op r@Ref {}))   =
  do (_ :/\ n) <- accumulate $ BBOp op (P :/\ l) (P :/\ r)
     findChoice (InBool n parent)

findChoice (InNum l@Ref' {} (InLB parent op r@Ref' {})) =
  do (_ :/\ n) <- accumulate  $ IBOp op (P :/\ l) (P :/\ r)
     findChoice (InBool n parent)

findChoice (InNum l@Ref' {} (InL' parent op r@Ref' {})) =
  do (_ :/\ n) <- iAccumulate' $ IIOp op (P :/\ l) (P :/\ r)
     findChoice (InNum n parent)

  -- folds
findChoice (InBool r@Ref{} (InU o e))   =
  do (_ :/\ n) <- accumulate $! BOp o (P :/\ r)
     findChoice (InBool n e)
findChoice (InNum r@Ref'{}  (InU' o e)) =
  do (_ :/\ n) <- iAccumulate' $! IOp o (P :/\ r)
     findChoice (InNum n e)

findChoice (InBool r@Ref {} (InR acc op parent)) =
  do (_ :/\ n) <- accumulate $! BBOp op (P :/\ Ref acc) (P :/\ r)
     findChoice (InBool n parent)

findChoice (InNum r@Ref' {} (InRB acc op parent)) =
  do (_ :/\ n) <- accumulate $! IBOp op (P :/\ Ref' acc) (P :/\ r)
     findChoice (InBool n parent)

findChoice (InNum r@Ref' {} (InR' acc op parent)) =
  do (_ :/\ n) <- iAccumulate' $! IIOp op (P :/\ Ref' acc) (P :/\ r)
     findChoice (InNum n parent)
  -- switch
findChoice (InBool (Ref l) (InL parent op r))   = findChoice (InBool r $ InR l op parent)
findChoice (InNum  (Ref' l) (InLB parent op r)) = findChoice (InNum  r $ InRB l op parent)
findChoice (InNum  (Ref' l) (InL' parent op r)) = findChoice (InNum  r $ InR' l op parent)
  -- recur
findChoice (InBool (BBOp op (_ :/\ l) (_ :/\ r)) ctx) = findChoice (InBool l $ InL ctx op r)
findChoice (InBool (IBOp op (_ :/\ l) (_ :/\ r)) ctx) = findChoice (InNum  l $ InLB ctx op r)
findChoice (InBool (BOp  o  (_ :/\ e))           ctx) = findChoice (InBool e $ InU o ctx)
findChoice (InNum  (IOp  o  (_ :/\ e))           ctx) = findChoice (InNum  e $ InU' o ctx)
findChoice (InNum  (IIOp op (_ :/\ l) (_ :/\ r)) ctx) = findChoice (InNum  l $ InL' ctx op r)
  -- legal to discharge Units under a conjunction only
findChoice (InBool Unit (InL parent And r))   = do t <- sTrue
                                                   findChoice (InBool r $ InR t And parent)
findChoice (InBool Unit (InR acc And parent)) = findChoice (InBool (Ref acc) parent)
  -- TODO Not sure if unit can ever exist with a context, most of these will
  -- never pass the parser and cannot be constructed in the Prop' data type, but
  -- the IL allows for them
findChoice x@(InBool Ref{} InU'{}) = error $ "Numeric unary operator applied to boolean: " ++ show x
findChoice x@(InNum Ref'{} InU{})  = error $ "Boolean unary operator applied to numeric: " ++ show x
findChoice (InBool Unit ctx)       = error $ "Unit with a context" ++ show ctx
findChoice x@(InNum Ref'{} Top)    = error $ "Numerics can only exist in SMT within a relation: " ++ show x
findChoice x@(InBool Ref{} InLB{}) = error $ "An impossible case bool reference in inequality: "  ++ show x
findChoice x@(InBool Ref{} InRB{}) = error $ "An impossible case bool reference in inequality: "  ++ show x
findChoice x@(InBool Ref{} InL'{}) = error $ "An impossible case bool reference in arithmetic: " ++ show x
findChoice x@(InBool Ref{} InR'{}) = error $ "An impossible case bool reference in arithmetic: "  ++ show x
findChoice x@(InNum Ref'{} InL{})  = error $ "An impossible case: " ++ show x
findChoice x@(InNum Ref'{} InR{})  = error $ "An impossible case: " ++ show x


store ::
  ( R.MonadReader State io
  ,  MonadIO            io
  , Z.MonadZ3           io
  ) => (Maybe VariantContext :/\  (Z.Result :/\ [(Var :/\ Value)])) -> io ()
-- if we use update like this we get the following ghc bug:
{- Running 1 benchmarks...
Benchmark auto: RUNNING...
auto: internal error: PAP object (0x4200b2c8d8) entered!
    (GHC version 8.10.2 for x86_64_unknown_linux)
    Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
Benchmark auto: ERROR
cabal: Benchmarks failed for bench:auto from vsmt-0.0.1.
store !r = update results (r <>)
-}
store r = asks (unResult . results)
          >>= liftIO . STM.atomically . flip STM.modifyTVar' (pure r <>)

-- | TODO newtype this maybe stuff, this is an alternative instance
mergeVC :: Maybe VariantContext -> Maybe VariantContext -> Maybe VariantContext
mergeVC Nothing Nothing    = Nothing
mergeVC a@(Just _) Nothing = a
mergeVC Nothing b@(Just _) = b
mergeVC (Just l) (Just r)  = Just $ l &&& r

-- | A function that enforces each configuration is updated in sync
updateConfigs :: (MonadIO m, R.MonadReader State m) =>
  Prop' Dim -> (Dim, Bool) -> SVariantContext -> m ()
updateConfigs context (d,val) sConf = do
  -- update the variant context
  update vConfig (mergeVC (Just $ VariantContext context))
  -- update the dimension cache
  update config (add d val)
  -- update the symbolic config
  update sConfig (const sConf)

-- | Reset the state but maintain the cache's. Notice that we only identify the
-- items which _should not_ reset and force those to be maintained
resetTo :: (R.MonadReader State io, MonadIO io) => FrozenStores -> io ()
{-# INLINE     resetTo #-}
{-# SPECIALIZE resetTo :: FrozenStores -> Solver () #-}
resetTo FrozenStores{..} = do update config  (const fconfig)
                              update sConfig (const fsConfig)
                              update vConfig (const fvConfig)

-- | Given a dimensions and a way to continue with the left alternative, and a
-- way to continue with the right alternative. Spawn two new subprocesses that
-- process the alternatives plugging the choice hole with its respective
alternative ::
  ( MonadIO     n
  , MonadLogger n
  , Z.MonadZ3   n
  ) => Dim -> SolverT n () -> SolverT n () -> SolverT n ()
alternative dim goLeft goRight =
  do !s <- freeze
     symbolicContext <- reads sConfig
     chans <- R.asks channels
     logInProducerWith "In alternative with Dim" dim
     let (fromVC, toVC) = getMainChans . mainChans $ chans
         dontNegate          = False
         pleaseNegate        = True


     -- When we see a new dimension we check if both of its possible
     -- bindings is satisfiable, if so then we proceed to compute the
     -- variant, if not then we skip. This happens twice because
     -- dimensions and variant contexts can only be booleans.
     logInProducer "Checking dim true"
     (checkDimTrue,!newSConfigL) <- liftIO $
       U.writeChan toVC (dim, dontNegate, symbolicContext) >> U.readChan fromVC
     when checkDimTrue $ do
       let !continueLeft = Z.local $
                           do logInProducerWith "Left Alternative of" dim
                              resetTo s
                              updateConfigs (bRef dim) (dim,True) newSConfigL
                              goLeft
       logInProducer "Writing to continue left"
       continueLeft


     logInProducer "Now left side"
     -- resetTo s

     -- right side, notice that we negate the symbolic, and reset the state
     (checkDimFalse,!newSConfigR) <- liftIO $
       U.writeChan toVC (dim, pleaseNegate, symbolicContext) >> U.readChan fromVC
     when checkDimFalse $ do
       let !continueRight = Z.local $
                            do logInProducerWith "Right Alternative of" dim
                               resetTo s
                               updateConfigs (bnot $ bRef dim) (dim,False) newSConfigR
                               goRight
       continueRight

removeChoices ::
  ( MonadLogger m
  , Z.MonadZ3   m
  ) => VarCore -> SolverT m ()
removeChoices (VarCore Unit) = reads bools >>= logInProducerWith "Core reduced to Unit" >>
                               reads config >>= logInProducerWith "Core reduced to Unit with Context" >>
                               do vC <- reads vConfig
                                  r  <- getResult
                                  store (vC :/\ r)
removeChoices (VarCore x@(Ref _)) = evaluate x >>= removeChoices
removeChoices (VarCore l) = choose (toLoc l)

choose ::
  ( MonadLogger m
  , Z.MonadZ3   m
  ) => Loc -> SolverT m ()
choose (InBool Unit Top)  = logInProducer "Choosing all done" >>
                               do vC <- reads vConfig
                                  r  <- getResult
                                  store (vC :/\ r)
choose (InBool l@Ref{} _) = logInProducer "Choosing all done" >>
                              evaluate l >>= removeChoices
choose loc =
  do
    loc' <- findChoice loc
    case loc' of
      (InBool (Chc d cl cr) ctx) -> do
        conf <- reads config
        -- we'd like to evaluate after IL but this makes the async harder so we
        -- accumulate instead. Specifically there is an interaction with in new
        -- assertion stack. When requests come out of order the assertion stack
        -- scope is also out of order, because evaluation relies on this
        -- ordering we cannot use it.
        logInProducer "Choosing Context"
        let goLeft  = toIL cl >>= accumulate . sSnd >>= findChoice . toLocWith ctx . sSnd >>= choose
            goRight = toIL cr >>= accumulate . sSnd >>= findChoice . toLocWith ctx . sSnd >>= choose

        case find d conf of
          Just True  -> -- logInProducer "Cache hit --- Left Selected"  >>
                        goLeft
          Just False -> -- logInProducer "Cache hit --- Right Selected" >>
                        goRight
          Nothing    -> do logInProducer "Cache miss running alt";
                           alternative d goLeft goRight

      (InNum (Chc' d cl cr) ctx) -> do
        logInProducer "Got choice in context InNum"
        conf <- reads config
        let goLeft  = toIL' cl >>= iAccumulate' . sSnd >>= findChoice . toLocWith' ctx . sSnd >>= choose
            goRight = toIL' cr >>= iAccumulate' . sSnd >>= findChoice . toLocWith' ctx . sSnd >>= choose

        case find d conf of
          Just True  -> logInProducer "Cache hit --- Left Selected"  >> goLeft
          Just False -> logInProducer "Cache hit --- Right Selected" >> goRight
          Nothing    -> alternative d goLeft goRight

      x -> error $ "Choosing and missed cases with: " ++ show x


--------------------------- Variant Context Helpers ----------------------------
contextToSBool :: ( Monad m
                   , Cachable m Dim SDimension
                   , Z.MonadZ3 m
                   ) => VariantContext -> m SDimension
contextToSBool (getVarFormula -> x) = go x
  where -- go :: Show a => Prop' a -> m SDimension
        go (LitB True)  = sDTrue
        go (LitB False) = sDFalse
        go (RefB d)     = cached d
        go (OpB Not e) = fmap SDimension $ go e >>= Z.mkNot . unSDimension
        go (OpBB op l r) = do l' <- go l
                              r' <- go r
                              dispatchDOp op l' r'
        go OpIB {} = error "numeric expressions are invalid in variant context"
        go ChcB {} = error "variational expressions are invalid in variant context"


instance Pretty V where
  pretty P = "plain"
  pretty V = "var"

instance Pretty Z.AST where pretty = Text.pack . show
instance Pretty SBool where pretty (unSBool -> b) = pretty b
instance (Pretty a, Pretty b) => Pretty (a :/\ b) where
  pretty (a :/\ b) = "(" <> pretty a <> "," <> pretty b <> ")"

instance Pretty IL where
  pretty Unit = "unit"
  pretty (Ref b)      = pretty b
  pretty (BOp Not r@(_ :/\ Ref _))   = "~" <> pretty r
  pretty (BOp o e)   = pretty o <> parens (pretty e)
  pretty (BBOp op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (IBOp op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (Chc d l r)  = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"

instance Pretty NRef where
  pretty (SI _) = "sInt"
  pretty (SD _) = "sDouble"

instance Pretty IL' where
  pretty (Ref' b)     = pretty b
  pretty (IOp o x@(_ :/\ Ref' _))  = pretty o <> pretty x
  pretty (IOp Abs e)  = between "|" (pretty e) "|"
  pretty (IOp o e)    = pretty o <> parens (pretty e)
  pretty (IIOp o l r) = parens $ mconcat [pretty l, " ", pretty o, " ", pretty r]
  pretty (Chc' d l r) = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"
