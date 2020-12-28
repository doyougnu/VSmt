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
import qualified Data.SBV.Internals                    as I
import qualified Data.SBV.Trans                        as T
import qualified Data.SBV.Trans.Control                as C
import qualified Data.Text                             as Text
import           GHC.Generics                          (Generic)

import           Data.Text.IO                          (putStrLn)
import           System.Mem.StableName                 (StableName, makeStableName)
import           Prelude                               hiding (EQ, GT, LT, log,
                                                        putStrLn,read,reads)


import           Core.Pretty
import           Core.Result
import           Core.Types

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
findVCore :: (MonadLogger m, I.SolverContext m) => IL -> m VarCore
findVCore = evaluate

solveVerbose :: Proposition -> Maybe VariantContext -> Settings -> IO Result
solveVerbose = internalSolver runPreSolverLog runSolverLog

solve :: Proposition -> Maybe VariantContext -> Settings -> IO Result
solve = internalSolver runPreSolverNoLog runSolverNoLog

-- TODO fix this horrendous type signature
internalSolver ::
  ( Constrainable m1 (ExRefType Var) IL'
  , Constrainable m1 Var IL
  , MonadLogger m1
  , MonadLogger m2
  , C.MonadQuery m2
  , T.MonadSymbolic m2
  , I.SolverContext m2
  ) =>
  (Stores -> m1 IL -> T.SymbolicT IO (IL, Stores))
  -> (State -> SolverT m2 Result -> C.QueryT IO (b1, b2))
  -> Prop' Var
  -> Maybe VariantContext
  -> Settings
  -> IO b1
internalSolver preSlvr slvr i conf Settings{..} = do
  (toMain, fromVC)   <- U.newChan vcBufSize
  (toVC,   fromMain) <- U.newChan vcBufSize
  initialStore       <- newStore
  initialCache       <- newCaches


  let solverConfig    = getConfig solver
      il              = toIL i

  -- init the channels
      vcChans   = VCChannels     $ (fromMain, toMain)
      mainChans = MainChannels   $ (fromVC, toVC)
      chans     = Channels{ vcChans=vcChans, mainChans=mainChans, threadId=0}
      startState  = State{stores=initialStore, channels=chans, caches=initialCache}
      seasoning = preSlvr initialStore (snd <$> il)

      runVCWorkers =
        A.mapConcurrently_ (vcWorker solverConfig conf startState slvr)
        [1..numVCWorkers]

      -- this thread will exit once it places requests on the producer
      -- chans. If the IL is a unit then it'll be caught by evaluate and
      -- placed on a result chan anyway
      populateChans = T.runSMTWith solverConfig $
        do (il', st) <- seasoning
           C.query $
             slvr startState{stores=st} $
             do void $ findVCore il' >>= removeChoices
                reads results

  -- kick off
  !aWorkers  <- A.async runVCWorkers

  (results, _) <- populateChans

  A.cancel aWorkers

  return results

-- solveForCoreVerbose :: Proposition -> Maybe VariantContext -> IO (VarCore, State s)
-- solveForCoreVerbose  i (fromMaybe true -> conf) =
--     T.runSMTWith T.z3{T.verbose=True} $
--       do (_,iState) <- runPreSolverLog mempty $ contextToSBool' conf
--          (il, st) <- runPreSolverLog iState $ toIL i
--          C.query $ runSolverLog st $
--            do core <- findVCore il
--               logWith "Proposition: "  i
--               logWith "Core: "         core
--               logWith "Is Core Unit: " (isUnit core)
--               return core

------------------------------ Async Helpers -----------------------------------
-- | season the solver, that is prepare it for async workloads
type Seasoning     = T.Symbolic (IL, Stores)
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
vcWorker :: (MonadLogger f, C.MonadQuery f,
             Constrainable f Dim (T.SBV Bool), I.SolverContext f, MonadIO f) =>
  T.SMTConfig
  -> Maybe VariantContext
  -> State
  -> (State -> f b -> C.QueryT IO a)
  -> Int
  -> IO ()
vcWorker c Nothing s@State{..} slvr tid =
  let (fromMain, toMain) = getVcChans . vcChans $ channels
  in T.runSMTWith c $ vcHelper fromMain toMain s slvr tid
vcWorker c (Just vc) s@State{..} slvr tid =
  T.runSMTWith c $ do
  -- season the solver
  let (fromMain, toMain) = getVcChans . vcChans $ channels
  (b,st) <- runPreSolverLog stores $ contextToSBool' vc
  T.constrain b
  -- now run the helper
  vcHelper fromMain toMain s{stores=st} slvr tid

vcHelper :: (C.ExtractIO m, MonadLogger f, C.MonadQuery f,
             Constrainable f a1 (T.SBV Bool), I.SolverContext f, MonadIO f) =>
  U.OutChan (a1, Bool, T.SBV Bool)
  -> U.InChan (Bool, T.SBV Bool)
  -> t
  -> (t -> f b -> C.QueryT m a2)
  -> Int
  -> T.SymbolicT m ()
vcHelper fromMain toMain st slvr tid =
  C.query $
  -- listen to the channel forever and check requests incrementally against the
  -- context if there is one
    void $
    slvr st $
    forever $
     do
       logInVC' tid "Waiting"
       (d, shouldNegate, vc') <- C.io $ U.readChan fromMain
        -- logInVC' tid "got request"
       C.inNewAssertionStack $
         do sD <- cached d
            let !new = if shouldNegate then bnot sD else sD &&& vc'
            T.constrain new
            C.checkSat >>= C.io . \case
              C.Sat -> U.writeChan toMain (True, new)
              _     -> U.writeChan toMain (False, new)

------------------------------ Data Types --------------------------------------
-- | Solver configuration is a mapping of dimensions to boolean values, we
-- express this in two ways, first we hold a store of dimensions to symbolic
-- booleans to track what we have seen, secondly we hold a variant context _as_
-- a symbolic formula rather than a data structure so that we can spin up a
-- separate thread to check variant context sat calls when removing choices
type Store = Map.HashMap

-- Stores of various things
type Ints         = Store Var T.SInteger
type Doubles      = Store Var T.SDouble
type Bools        = Store Var T.SBool
type Dimensions   = Store Dim T.SBool
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

type SVariantContext = T.SBool
instance Semigroup SVariantContext where (<>) = (&&&)
instance Monoid    SVariantContext where mempty = true


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
  }

type Cache a b = Map.HashMap (StableName a) b
type ACache = Cache IL (V,IL)

data Caches = Caches
              { accCache :: !(STM.TVar ACache)
              }

data Stores = Stores
    { vConfig    :: !(STM.TVar (Maybe VariantContext)) -- the formula representation of the config
    , sConfig    :: !(STM.TVar SVariantContext)        -- symbolic representation of a config
    , config     :: !(STM.TVar Context)                -- a map or set representation of the config
    , ints       :: !(STM.TVar Ints)
    , doubles    :: !(STM.TVar Doubles)
    , bools      :: !(STM.TVar Bools)
    , dimensions :: !(STM.TVar Dimensions)
    , results    :: !(STM.TVar Result)
    }

data FrozenStores = FrozenStores
    { fvConfig    :: !(Maybe VariantContext) -- the formula representation of the config
    , fsConfig    :: !SVariantContext        -- symbolic representation of a config
    , fconfig     :: !Context                -- a map or set representation of the config
    , fints       :: !Ints
    , fdoubles    :: !Doubles
    , fbools      :: !Bools
    , fdimensions :: !Dimensions
    , fresults    :: !Result
    }

data Channels = Channels { vcChans   :: VCChannels
                         , mainChans :: MainChannels
                         , threadId  :: !ThreadID
                         }
newCaches :: IO Caches
newCaches = do accCache <- STM.newTVarIO mempty
               return Caches{..}

newStore :: IO Stores
newStore = do vConfig    <- STM.newTVarIO mempty
              sConfig    <- STM.newTVarIO mempty
              config     <- STM.newTVarIO mempty
              ints       <- STM.newTVarIO mempty
              doubles    <- STM.newTVarIO mempty
              bools      <- STM.newTVarIO mempty
              results    <- STM.newTVarIO mempty
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
updateWith field f = R.asks field >>= liftIO . STM.atomically . flip STM.modifyTVar' f

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
            fresults <- read results st
            fdimensions <- read dimensions st
            return FrozenStores{..}

-- TODO remove the StateT dependency for ReaderT
-- | A solver is just a reader over a solver enabled monad. The reader
-- maintains information during the variational execution, such as
-- configuration, variable stores
newtype SolverT m a = SolverT { runSolverT :: R.ReaderT State m a }
  deriving ( Functor,Applicative,Monad,MonadIO
           , MonadError e, MonadLogger, R.MonadReader State
           , T.MonadSymbolic, C.MonadQuery, MonadTrans
           )

-- mapSolverT :: (m (a1, Stores) -> m (a2, Stores)) -> SolverT m a1 -> SolverT m a2
mapSolverT :: R.MonadReader r m => (r -> r) -> SolverT m a -> SolverT m a
mapSolverT f = SolverT . R.mapReaderT (local f) . runSolverT

-- | Unfortunately we have to write this one by hand. This type class tightly
-- couples use to SBV and is the mechanism to constrain things in the solver
instance (Monad m, I.SolverContext m) => I.SolverContext (SolverT m) where
  constrain       = lift . T.constrain
  softConstrain   = lift . T.softConstrain
  setOption       = lift . T.setOption
  namedConstraint = (lift .) . T.namedConstraint
  addAxiom        = (lift .) . T.addAxiom
  contextState    = lift I.contextState
  constrainWithAttribute = (lift .) . T.constrainWithAttribute

instance (Monad m, I.SolverContext m) => I.SolverContext (LoggingT m) where
  constrain       = lift . T.constrain
  softConstrain   = lift . T.softConstrain
  setOption       = lift . T.setOption
  namedConstraint = (lift .) . T.namedConstraint
  addAxiom        = (lift .) . T.addAxiom
  contextState    = lift I.contextState
  constrainWithAttribute = (lift .) . T.constrainWithAttribute

instance (Monad m, I.SolverContext m) => I.SolverContext (NoLoggingT m) where
  constrain       = lift . T.constrain
  softConstrain   = lift . T.softConstrain
  setOption       = lift . T.setOption
  namedConstraint = (lift .) . T.namedConstraint
  addAxiom        = (lift .) . T.addAxiom
  contextState    = lift I.contextState
  constrainWithAttribute = (lift .) . T.constrainWithAttribute


instance C.MonadQuery m    => C.MonadQuery (LoggingT m)      where
  queryState  = lift C.queryState
instance T.MonadSymbolic m => T.MonadSymbolic (LoggingT m)   where
  symbolicEnv = lift T.symbolicEnv
instance C.MonadQuery m    => C.MonadQuery (NoLoggingT m)    where
  queryState  = lift C.queryState
instance T.MonadSymbolic m => T.MonadSymbolic (NoLoggingT m) where
  symbolicEnv = lift T.symbolicEnv

-- | A solver type enabled with query operations and logging
type SolverLog    = SolverT    (LoggingT   C.Query)
type Solver       = SolverT    (NoLoggingT C.Query)
type PreSolverLog = PreSolverT (LoggingT   T.Symbolic)
type PreSolver    = PreSolverT (NoLoggingT T.Symbolic)

-- | A presolver runs the first stage of the evaluation/accumulation loop, that
-- is, it is a solver which doesn't understand async, nor incremental push/pops.
-- Rather, it is the solver which generates the first core
newtype PreSolverT m a = PreSolverT { runPreSolverT :: (R.ReaderT Stores m) a }
  deriving ( Functor,Applicative,Monad,MonadIO
           , MonadError e, MonadLogger, R.MonadReader Stores
           , T.MonadSymbolic, C.MonadQuery
           )

runPreSolverLog :: Stores -> PreSolverLog a -> T.Symbolic (a, Stores)
runPreSolverLog s = fmap (,s) . runStdoutLoggingT . flip R.runReaderT s . runPreSolverT

runPreSolverNoLog :: Stores -> PreSolver a -> T.Symbolic (a, Stores)
runPreSolverNoLog s = fmap (,s) . runNoLoggingT . flip R.runReaderT s . runPreSolverT

runSolverNoLog :: State -> Solver a -> C.Query (a, State)
runSolverNoLog s = fmap (,s) . runNoLoggingT . flip R.runReaderT s . runSolverT

runSolverLog :: State -> SolverLog a -> C.Query (a, State)
runSolverLog s = fmap (,s) . runStdoutLoggingT . flip R.runReaderT s . runSolverT

type PreRun m a = Stores -> m a -> T.Symbolic (a, Stores)
type Run m    a = State  -> SolverT m a -> C.Query    (a, State)

class RunPreSolver s where
  runPreSolver :: Stores -> s a -> T.Symbolic (a, Stores)

class RunSolver s where
  runSolver :: State -> s a -> C.Query (a, State)

instance RunSolver SolverLog       where runSolver    = runSolverLog
instance RunSolver Solver          where runSolver    = runSolverNoLog
instance RunPreSolver PreSolverLog where runPreSolver = runPreSolverLog
instance RunPreSolver PreSolver    where runPreSolver = runPreSolverNoLog

class Show a => Constrainable m a b where cached :: a -> m b

-- -- TODO fix this duplication with derivingVia
instance (Monad m, T.MonadSymbolic m, C.MonadQuery m, MonadLogger m) =>
  Constrainable (SolverT m) Var IL where
  cached ref = do
    st <- reads bools
    case find ref st of
      Just x -> logWith "Cache Hit" ref >> return (Ref x)
      Nothing -> do
        logInProducerWith "Cache miss on" ref
        newSym <- T.label (Text.unpack ref) <$> C.freshVar (Text.unpack ref)
        update bools (add ref newSym)
        return (Ref newSym)

instance (MonadLogger m, Monad m, T.MonadSymbolic m, C.MonadQuery m) =>
  Constrainable (SolverT m) Dim T.SBool where
  cached d = do
    st <- reads dimensions
    case find d st of
      Just x -> return x
      Nothing -> do
        let ref = Text.unpack $ getDim d
        newSym <- T.label ref <$> C.freshVar ref
        update dimensions (add d newSym)
        return newSym

instance (Monad m, T.MonadSymbolic m, C.MonadQuery m) =>
  Constrainable (SolverT m) (ExRefType Var) IL' where
  cached (ExRefTypeI i) =
    do st <- reads ints
       case find i st of
         Just x  -> return . Ref' . SI $ x
         Nothing -> do newSym <- T.label (Text.unpack i) <$> C.freshVar (Text.unpack i)
                       update ints (add i newSym)
                       return (Ref' . SI $ newSym)

  cached (ExRefTypeD d) =
    do st <- reads doubles
       case find d st of
         Just x  -> return . Ref' $ SD x
         Nothing -> do newSym <- T.label (Text.unpack d) <$> C.freshVar (Text.unpack d)
                       update doubles (add d newSym)
                       return $! Ref' $ SD newSym

instance (Monad m, T.MonadSymbolic m, MonadLogger m) =>
  Constrainable (PreSolverT m) Var IL where
  cached ref   = do st <- readWith bools
                    case find ref st of
                      Just x  -> return (Ref x)
                      Nothing -> do newSym <- T.sBool (Text.unpack ref)
                                    updateWith bools (add ref newSym)
                                    return (Ref newSym)


instance (Monad m, T.MonadSymbolic m) =>
  Constrainable (PreSolverT m) (ExRefType Var) IL' where
  cached (ExRefTypeI i) =
    do st <- readWith ints
       case find i st of
         Just x  -> return . Ref' . SI $ x
         Nothing -> do newSym <- T.sInteger (Text.unpack i)
                       updateWith ints (add i newSym)
                       return (Ref' . SI $ newSym)

  cached (ExRefTypeD d) =
    do st <- readWith doubles
       case find d st of
         Just x  -> return . Ref' $ SD x
         Nothing -> do newSym <- T.sDouble (Text.unpack d)
                       updateWith doubles (add d newSym)
                       return $! Ref' $ SD newSym

instance (Monad m, T.MonadSymbolic m) =>
  Constrainable (PreSolverT m) Dim T.SBool where
  cached d = do
    ds <- readWith dimensions
    case find d ds of
      Just x -> return x
      Nothing -> do
        let ref = Text.unpack $ getDim d
        newSym <- T.sBool ref
        updateWith dimensions (add d newSym)
        return newSym

-- | A general caching mechanism using StableNames. There is a small chance of a
-- collision ~32k per 2^24. I leave this completely unhandled as it is so rare I
-- doubt it'll ever actually occur
instance (Monad m, MonadIO m) => Constrainable (SolverT m) IL (V,IL) where
  cached !il = do ch <- readCache accCache
                  sn <- liftIO $! il `seq` makeStableName il
                  case find sn ch of
                    Just x  -> return x
                    Nothing -> do let !val = iAccumulate il
                                  updateCache accCache (add sn val)
                                  return val

----------------------------------- IL -----------------------------------------
type BRef = T.SBool

data NRef = SI T.SInteger
    | SD T.SDouble
    deriving (Generic,Show,Eq)

instance Num NRef where
  fromInteger = SI . T.literal . fromInteger

  abs (SI i) = SI $ abs i
  abs (SD d) = SD $ T.fpAbs d

  negate (SI i) = SI $ negate i
  negate (SD d) = SD $ T.fpNeg d

  signum (SI i) = SI $ signum i
  signum (SD d) = SD $ signum (T.fromSDouble T.sRoundNearestTiesToAway d)

  (SI i) + (SI i') = SI $ i + i'
  (SD d) + (SI i)  = SD $ d + T.sFromIntegral i
  (SI i) + (SD d)  = SD $ T.sFromIntegral i + d
  (SD d) + (SD d') = SD $ d + d'

  (SI i) - (SI i') = SI $ i - i'
  (SD d) - (SI i)  = SD $ d - T.sFromIntegral i
  (SI i) - (SD d)  = SD $ T.sFromIntegral i - d
  (SD d) - (SD d') = SD $ d - d'

  (SI i) * (SI i') = SI $ i * i'
  (SD d) * (SI i)  = SD $ d * T.sFromIntegral i
  (SI i) * (SD d)  = SD $ d * T.sFromIntegral i
  (SD d) * (SD d') = SD $ d * d'

instance PrimN NRef where
  (SI i) ./ (SI i') = SI $ i ./ i'
  (SD d) ./ (SI i)  = SD $ d ./ T.sFromIntegral i
  (SI i) ./ (SD d)  = SD $ T.sFromIntegral i ./ d
  (SD d) ./ (SD d') = SD $ d ./ d'


  (SI i) .% (SI i') = SI $ i .% i'
  (SD d) .% (SI i)  = SI $ T.fromSDouble T.sRoundNearestTiesToAway d .% i
  (SI i) .% (SD d)  = SI $ i .% T.fromSDouble T.sRoundNearestTiesToAway d
  (SD d) .% (SD d') = SD $ T.fpRem d d'

instance T.Mergeable NRef where
  symbolicMerge _ b thn els
    | Just res <- T.unliteral b = if res then thn else els
    | otherwise = els

instance T.EqSymbolic NRef where
  (.==) (SI i) (SI i') = (T..==) i i'
  (.==) (SD d) (SI i') = (T..==) d (T.sFromIntegral i')
  (.==) (SI i) (SD d)  = (T..==) (T.sFromIntegral i) d
  (.==) (SD d) (SD d') = (T..==) d d'

  (./=) (SI i) (SI i') = (T../=) i i'
  (./=) (SD d) (SI i') = (T../=) d (T.sFromIntegral i')
  (./=) (SI i) (SD d)  = (T../=) (T.sFromIntegral i) d
  (./=) (SD d) (SD d') = (T../=) d d'

instance T.OrdSymbolic NRef where
  (.<) (SI i) (SI i') = (T..<) i i'
  (.<) (SD d) (SI i)  = (T..<) d (T.sFromIntegral i)
  (.<) (SI i) (SD d)  = (T..<) (T.sFromIntegral i) d
  (.<) (SD d) (SD d') = (T..<) d d'

  (.<=) (SI i) (SI i') = (T..<=) i i'
  (.<=) (SD d) (SI i)  = (T..<=) d (T.sFromIntegral i)
  (.<=) (SI i) (SD d)  = (T..<=) (T.sFromIntegral i) d
  (.<=) (SD d) (SD d') = (T..<=) d d'

  (.>=) (SI i) (SI i') = (T..>=) i i'
  (.>=) (SD d) (SI i)  = (T..>=) d (T.sFromIntegral i)
  (.>=) (SI i) (SD d)  = (T..>=) (T.sFromIntegral i) d
  (.>=) (SD d) (SD d') = (T..>=) d d'

  (.>) (SI i) (SI i') = (T..>) i i'
  (.>) (SD d) (SI i)  = (T..>) d (T.sFromIntegral i)
  (.>) (SI i) (SD d)  = (T..>) (T.sFromIntegral i) d
  (.>) (SD d) (SD d') = (T..>) d d'

instance Prim T.SBool NRef where
  (.<) (SI i) (SI i') = (T..<) i i'
  (.<) (SD d) (SI i)  = (T..<) d (T.sFromIntegral i)
  (.<) (SI i) (SD d)  = (T..<) (T.sFromIntegral i) d
  (.<) (SD d) (SD d') = (T..<) d d'

  (.<=) (SI i) (SI i') = (T..<=) i i'
  (.<=) (SD d) (SI i)  = (T..<=) d (T.sFromIntegral i)
  (.<=) (SI i) (SD d)  = (T..<=) (T.sFromIntegral i) d
  (.<=) (SD d) (SD d') = (T..<=) d d'

  (.>=) (SI i) (SI i') = (T..>=) i i'
  (.>=) (SD d) (SI i)  = (T..>=) d (T.sFromIntegral i)
  (.>=) (SI i) (SD d)  = (T..>=) (T.sFromIntegral i) d
  (.>=) (SD d) (SD d') = (T..>=) d d'

  (.>) (SI i) (SI i') = (T..>) i i'
  (.>) (SD d) (SI i)  = (T..>) d (T.sFromIntegral i)
  (.>) (SI i) (SD d)  = (T..>) (T.sFromIntegral i) d
  (.>) (SD d) (SD d') = (T..>) d d'

  (.==) (SI i) (SI i') = (T..==) i i'
  (.==) (SD d) (SI i') = (T..==) d (T.sFromIntegral i')
  (.==) (SI i) (SD d)  = (T..==) (T.sFromIntegral i) d
  (.==) (SD d) (SD d') = (T..==) d d'

  (./=) (SI i) (SI i') = (T../=) i i'
  (./=) (SD d) (SI i') = (T../=) d (T.sFromIntegral i')
  (./=) (SI i) (SD d)  = (T../=) (T.sFromIntegral i) d
  (./=) (SD d) (SD d') = (T../=) d d'

-- | The intermediate language, we express negation but negation will not exist
-- in a variational core. The IL language is intermediate and used to collapse
-- the input program to it's essential variational structure. We exploit the
-- symbolic references to represent plain sub-trees such that the only thing
-- that will survive in the variational core are binary connectives, symbolic
-- references and choices
data IL = Unit
    | Ref !BRef
    | BOp   B_B (V, IL)
    | BBOp BB_B (V, IL)  (V, IL)
    | IBOp NN_B (V, IL') (V, IL')
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
    | IOp   N_N (V, IL')
    | IIOp NN_N (V, IL') (V, IL')
    | Chc' Dim NExpression NExpression
    deriving (Generic, Show, Eq)

-- TODO: factor out the redundant cases into a type class
-- | Convert a proposition into the intermediate language to generate a
-- Variational Core
toIL ::
  ( MonadLogger   m
  , Constrainable m (ExRefType Var) IL'
  , Constrainable m Var IL
  ) => Prop' Var -> m (V, IL)
toIL (LitB True)   = return $! (P, Ref T.sTrue)
toIL (LitB False)  = return $! (P, Ref T.sFalse)
toIL (RefB ref)    = (P,) <$> cached ref
toIL (OpB op e)      = do (v,e') <- toIL e; return (v, BOp op (v, e'))
toIL (OpBB op l r) = do l'@(vl, _) <- toIL l
                        r'@(vr, _) <- toIL r
                        return $ (vl <@> vr, BBOp op l' r')
toIL (OpIB op l r) = do l'@(vl,_)  <- toIL' l
                        r'@(vr,_) <- toIL' r
                        return $ (vl <@> vr, IBOp op l' r')
toIL (ChcB d l r)  = return $ (V, Chc d l r)

toIL' :: (Constrainable m (ExRefType Var) IL'
         , MonadLogger m) =>
         NExpr' Var -> m (V, IL')
toIL' (LitI (I i))  = return . (P,) . Ref' . SI $ T.literal i
toIL' (LitI (D d))  = return . (P,) . Ref' . SD $ T.literal d
toIL' (RefI a)      = (P,) <$> cached a
toIL' (OpI op e)    = do e'@(v,_) <- toIL' e; return (v, IOp op e')
toIL' (OpII op l r) = do l'@(vl,_) <- toIL' l
                         r'@(vr,_) <- toIL' r
                         return $! (vl <@> vr, IIOp op l' r')
toIL' (ChcI d l r)  = return $ (V, Chc' d l r)

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

dispatchOp :: Boolean b => BB_B -> b -> b -> b
dispatchOp And  = (&&&)
dispatchOp Or   = (|||)
dispatchOp Impl = (==>)
dispatchOp Eqv  = (<=>)
dispatchOp XOr  = (<+>)

dispatchUOp' :: Num a => N_N -> a -> a
dispatchUOp' Neg  = negate
dispatchUOp' Abs  = abs
dispatchUOp' Sign = signum

dispatchIOp' :: PrimN a => NN_N -> a -> a -> a
dispatchIOp' Add  = (+)
dispatchIOp' Sub  = (-)
dispatchIOp' Div  = (./)
dispatchIOp' Mult = (*)
dispatchIOp' Mod  = (.%)

dispatchOp' :: T.OrdSymbolic a => NN_B -> a -> a -> T.SBool
dispatchOp' LT  = (T..<)
dispatchOp' LTE = (T..<=)
dispatchOp' EQ  = (T..==)
dispatchOp' NEQ = (T../=)
dispatchOp' GT  = (T..> )
dispatchOp' GTE = (T..>=)

-- | Accumulation: we purposefully are verbose to provide the optimizer better
-- opportunities. Accumulation seeks to combine as much as possible the plain
-- terms in the AST into symbolic references
iAccumulate :: IL -> (V, IL)
 -- computation rules
iAccumulate Unit                        = (P,Unit)
iAccumulate x@Ref{} = (P,x)
iAccumulate x@Chc{} = (V,x)
  -- bools
iAccumulate (BOp Not (_,Ref r))            = (P,) . Ref $! bnot r
iAccumulate (BBOp op (_,Ref l) (_,Ref r))   = (P,) . Ref $! (dispatchOp op) l r
  -- numerics
iAccumulate (IBOp op (_,Ref' l) (_,Ref' r))  = (P,) . Ref $! (dispatchOp' op) l r
  -- choices
iAccumulate x@(BBOp _ (_,Chc {}) (_,Chc {}))   = (V, x)
iAccumulate x@(BBOp _ (_,Ref _) (_,Chc {}))    = (V, x)
iAccumulate x@(BBOp _ (_,Chc {}) (_,Ref _))    = (V, x)
iAccumulate x@(IBOp _ (_,Chc' {}) (_,Chc' {})) = (V, x)
iAccumulate x@(IBOp _ (_,Ref' _) (_,Chc' {}))  = (V, x)
iAccumulate x@(IBOp _ (_,Chc' {}) (_,Ref' _))  = (V, x)
 -- congruence rules
iAccumulate (BOp Not (P, e)) = let (_,!e') = iAccumulate e
                                   res     = BOp Not (P,e')
                               in iAccumulate res
iAccumulate (BOp Not (V, e)) = let (_,!e') = iAccumulate e
                                   res     = BOp Not (V,e')
                               in (V,res)
iAccumulate (BBOp op (P,l) (P,r)) = let (_,!l')  = iAccumulate l
                                        (_,!r')  = iAccumulate r
                                        !res = BBOp op (P,l') (P,r')
                                    in iAccumulate res
iAccumulate (BBOp op (_,l) (_,r)) = let (vl,!l')  = iAccumulate l
                                        (vr,!r')  = iAccumulate r
                                        !res      = BBOp op (vl,l') (vr,r')
                                     in (vl <@> vr, res)
iAccumulate (IBOp op (P,l) (P,r)) = let (_,!l') = iAccumulate' l
                                        (_,!r') = iAccumulate' r
                                        !res = IBOp op (P,l') (P,r')
                                    in iAccumulate res
iAccumulate (IBOp op (_,l) (_,r)) = let x@(vl,_) = iAccumulate' l
                                        y@(vr,_) = iAccumulate' r
                                        !res     = IBOp op x y
                                     in (vl <@> vr, res)

iAccumulate' :: IL' -> (V, IL')
  -- computation rules
iAccumulate' x@(Ref' _)          = (P, x)
iAccumulate' (IOp op (_,Ref' n)) = (P,) . Ref' $! (dispatchUOp' op) n
iAccumulate' (IIOp op (_,Ref' l) (_,Ref' r))  = (P,) . Ref' $! (dispatchIOp' op) l r
  -- choices
iAccumulate' x@Chc' {}                     = (V, x)
iAccumulate' x@(IIOp _ (_,Chc' {}) (_,Chc' {}))    = (V, x)
iAccumulate' x@(IIOp _ (_,Ref' _) (_,Chc' {}))   = (V, x)
iAccumulate' x@(IIOp _ (_,Chc' {}) (_,Ref' _))   = (V, x)
iAccumulate' (IIOp op c@(_,Chc'{}) (P,r)) = let r' = iAccumulate' r
                                           in (V,IIOp op c r')
iAccumulate' (IIOp op (P,l) c@(_,Chc'{})) = let l' = iAccumulate' l
                                           in (V,IIOp op l' c)
  -- congruence rules
iAccumulate' (IOp o (P, e))  = let !e' = iAccumulate' e
                                   res = IOp o e'
                               in iAccumulate' res
iAccumulate' (IOp o (_, e))  = let (v,!e')  = iAccumulate' e
                                   res = IOp o (v,e')
                               in (v,res)

iAccumulate' (IIOp o (P,l) (P,r)) = let x  = iAccumulate' l
                                        y  = iAccumulate' r
                                        res = IIOp o x y
                                    in iAccumulate' res
iAccumulate' (IIOp o (_,l) (_,r)) = let x@(vl,_)  = iAccumulate' l
                                        y@(vr,_)  = iAccumulate' r
                                        res = IIOp o x y
                                    in (vl <@> vr, res)

-------------------------------- Evaluation -----------------------------------
toSolver :: (Monad m, I.SolverContext m) => T.SBool -> m VarCore
{-# INLINE toSolver #-}
toSolver a = do T.constrain a; return $! intoCore Unit

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
evaluate :: (MonadLogger m, I.SolverContext m) => IL -> m VarCore
evaluate Unit     = return $! intoCore Unit
evaluate (Ref b)  = toSolver b
evaluate x@Chc {} = return $! intoCore x
  -- bools
evaluate (BOp Not   (_,Ref r))         = toSolver $! bnot r
evaluate (BBOp op (_,Ref l) (_,Ref r)) = toSolver $! (dispatchOp op) l r
  -- numerics
evaluate (IBOp op  (_,Ref' l) (_,Ref' r)) = toSolver $! (dispatchOp' op) l r
  -- choices
evaluate x@(BBOp _ (V,_) (V,_))            = return $! intoCore x
evaluate x@(IBOp _ (V,_) (V,_))            = return $! intoCore x
  -- congruence cases
evaluate (BBOp And (_,l) (_,Unit))      = evaluate l
evaluate (BBOp And (_,Unit) (_,r))      = evaluate r
evaluate (BBOp And (_,l) (_,x@(Ref _))) = do _ <- evaluate x; evaluate l
evaluate (BBOp And (_,x@(Ref _)) (_,r)) = do _ <- evaluate x; evaluate r
evaluate (IBOp op (P,l) (P,r))        =
  let l' = iAccumulate' l
      r' = iAccumulate' r
      res = IBOp op l' r'
  in evaluate res

evaluate (IBOp op (_,l) (_,r))        =
  let l' = iAccumulate' l
      r' = iAccumulate' r
      res = IBOp op l' r' in
    return $! intoCore res

  -- accumulation cases
evaluate x@(BOp Not (P,_))  = let (_,x') = iAccumulate x in evaluate x'
evaluate x@(BOp Not (V,_))  = let (_,x') = iAccumulate x in return $! intoCore x'
evaluate (BBOp And (P,l) (P,r)) = log "[Eval P P] And case" >>
  do (VarCore l') <- evaluate l
     (VarCore r') <- evaluate r
     let !res = BBOp And (P,l') (P,r')
     evaluate res
evaluate (BBOp And (V,l) (P,r)) = log "[Eval V P] And case" >>
  do (VarCore r') <- evaluate r
     let !res = BBOp And (V,l) (P,r')
     evaluate res
evaluate (BBOp And (P,l) (V,r)) = log "[Eval P V] And case" >>
  do (VarCore l') <- evaluate l
     let !res = BBOp And (P,l') (V,r)
     evaluate res
evaluate (BBOp op (P,l) (P,r)) = log "[Eval P P] General Case" >>
  do let (_,l') = iAccumulate l
         (_,r') = iAccumulate r
     let res = BBOp op (P,l') (P,r')
     evaluate res
evaluate (BBOp op (V,l) (P,r)) = log "[Eval V P] General Case" >>
  let (_,r') = iAccumulate r
      res = BBOp op (V,l) (P,r') in
    return $! intoCore res
evaluate (BBOp op (P,l) (V,r)) = log "[Eval P V] General Case" >>
  let (_,l') = iAccumulate l
      res = BBOp op (P,l') (V,r) in
    return $! intoCore res

------------------------- Removing Choices -------------------------------------
-- TODO transform to a GADT
-- | We use a zipper to track the context when searching for choices, this
-- removes the need to perform tree rotations. We make this as strict as
-- possible because we know we will be consuming the entire structure so there
-- is not need to build thunks
data Ctx = InL  !Ctx      BB_B  IL
         | InR  !T.SBool  BB_B  !Ctx
         | InLB !Ctx      NN_B  IL'
         | InRB NRef      NN_B  !Ctx
         | InL' !Ctx      NN_N  IL'
         | InR' NRef      NN_N  !Ctx
         | InU            B_B   !Ctx
         | InU'           N_N   !Ctx
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

findChoice :: Loc -> Loc
  -- base cases
findChoice x@(InBool Ref{} Top) = x
findChoice x@(InBool Unit Top)  = x
findChoice x@(InBool Chc {} _)  = x
findChoice x@(InNum Chc' {} _)  = x
  -- discharge two references
findChoice (InBool l@Ref {} (InL parent op r@Ref {}))   = findChoice (InBool (snd . iAccumulate  $ BBOp op (P,l) (P,r)) parent)
findChoice (InNum l@Ref' {} (InLB parent op r@Ref' {})) = findChoice (InBool (snd . iAccumulate  $ IBOp op (P,l) (P,r)) parent)
findChoice (InNum l@Ref' {} (InL' parent op r@Ref' {})) = findChoice (InNum  (snd . iAccumulate' $ IIOp op (P,l) (P,r)) parent)
  -- fold
findChoice (InBool r@Ref{} (InU o e))   = findChoice (InBool (snd . iAccumulate $ BOp o (P,r)) e)
findChoice (InNum r@Ref'{}  (InU' o e)) = findChoice (InNum (snd . iAccumulate' $ IOp o (P,r)) e)
findChoice (InBool r@Ref {} (InR acc op parent))  =
  findChoice (InBool (snd . iAccumulate $ BBOp op (P,Ref acc) (P,r)) parent)
findChoice (InNum r@Ref' {} (InRB acc op parent)) =
  findChoice (InBool (snd . iAccumulate $ IBOp op (P,Ref' acc) (P,r)) parent)
findChoice (InNum r@Ref' {} (InR' acc op parent)) =
  findChoice (InNum (snd . iAccumulate' $ IIOp op (P,Ref' acc) (P,r)) parent)
  -- switch
findChoice (InBool (Ref l) (InL parent op r))   = findChoice (InBool r $ InR l op parent)
findChoice (InNum  (Ref' l) (InLB parent op r)) = findChoice (InNum  r $ InRB l op parent)
findChoice (InNum  (Ref' l) (InL' parent op r)) = findChoice (InNum  r $ InR' l op parent)
  -- recur
findChoice (InBool (BBOp op (_,l) (_,r)) ctx) = findChoice (InBool l $ InL ctx op r)
findChoice (InBool (IBOp op (_,l) (_,r)) ctx) = findChoice (InNum  l $ InLB ctx op r)
findChoice (InBool (BOp o (_,e)) ctx)     = findChoice (InBool e $ InU o ctx)
findChoice (InNum (IOp o (_,e)) ctx)      = findChoice (InNum e  $ InU' o ctx)
findChoice (InNum (IIOp op (_,l) (_,r)) ctx)  = findChoice (InNum  l $ InL' ctx op r)
  -- legal to discharge Units under a conjunction only
findChoice (InBool Unit (InL parent And r))   = findChoice (InBool r $ InR true And parent)
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
  ) => Result -> io ()
{-# INLINE     store #-}
{-# SPECIALIZE store :: Result -> Solver () #-}
store !r = update results (r <>)

-- | TODO newtype this maybe stuff, this is an alternative instance
mergeVC :: Maybe VariantContext -> Maybe VariantContext -> Maybe VariantContext
mergeVC Nothing Nothing    = Nothing
mergeVC a@(Just _) Nothing = a
mergeVC Nothing b@(Just _) = b
mergeVC (Just l) (Just r)  = Just $ l &&& r

-- | A function that enforces each configuration is updated in sync
updateConfigs :: (MonadIO m, R.MonadReader State m) =>
  Prop' Dim -> (Dim, Bool) -> SVariantContext -> m ()
{-# INLINE     updateConfigs #-}
{-# SPECIALIZE updateConfigs :: Prop' Dim -> (Dim, Bool) -> SVariantContext -> Solver () #-}
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
  ( T.MonadSymbolic          n
  , MonadIO                  n
  , C.MonadQuery             n
  , MonadLogger              n
  ) => Dim -> SolverT n () -> SolverT n () -> SolverT n ()
{-# SPECIALIZE alternative :: Dim -> Solver () -> Solver () -> Solver () #-}
alternative dim goLeft goRight =
  do !s <- freeze
     symbolicContext <- reads sConfig
     logInProducerWith "In alternative with Dim" dim
     chans <- R.asks channels
     let (fromVC, toVC) = getMainChans . mainChans $ chans
         dontNegate          = False
         pleaseNegate        = True


     -- When we see a new dimension we check if both of its possible
     -- bindings is satisfiable, if so then we proceed to compute the
     -- variant, if not then we skip. This happens twice because
     -- dimensions and variant contexts can only be booleans.
     (checkDimTrue,!newSConfigL) <- liftIO $
       U.writeChan toVC (dim, dontNegate, symbolicContext) >> U.readChan fromVC
     when checkDimTrue $ do
       let !continueLeft = C.inNewAssertionStack $!
                           do logInProducerWith "Left Alternative of" dim
                              resetTo s
                              reads config >>= logInProducerWith "ConfigL:"
                              reads bools  >>= logInProducerWith "Bools:"
                              updateConfigs (bRef dim) (dim,True) newSConfigL
                              goLeft
       logInProducer "Writing to continue left"
       continueLeft


     resetTo s

     -- right side, notice that we negate the symbolic, and reset the state
     (checkDimFalse,!newSConfigR) <- liftIO $
       U.writeChan toVC (dim, pleaseNegate, symbolicContext) >> U.readChan fromVC
     when checkDimFalse $ do
       let !continueRight = C.inNewAssertionStack $
                            do logInProducerWith "Right Alternative of" dim
                               resetTo s
                               reads config >>= logInProducerWith "ConfigR:"
                               reads bools  >>= logInProducerWith "BoolsR:"
                               updateConfigs (bnot $ bRef dim) (dim,False) newSConfigR
                               goRight
       continueRight

removeChoices ::
  ( MonadLogger     m
  , C.MonadQuery    m
  , I.SolverContext m
  , T.MonadSymbolic m
  ) => VarCore -> SolverT m ()
{-# INLINE     removeChoices #-}
{-# SPECIALIZE removeChoices :: VarCore -> Solver () #-}
removeChoices (VarCore Unit) = reads bools >>= logInProducerWith "Core reduced to Unit" >>
                               reads config >>= logInProducerWith "Core reduced to Unit with Context" >>
                               reads vConfig >>= getResult >>= store
removeChoices (VarCore x@(Ref _)) = evaluate x >>= removeChoices
removeChoices (VarCore l) = choose (toLoc l)

choose ::
  ( MonadLogger     m
  , I.SolverContext m
  , T.MonadSymbolic m
  , C.MonadQuery    m
  ) => Loc -> SolverT m ()
{-# INLINE     choose #-}
{-# SPECIALIZE choose :: Loc -> Solver () #-}
choose (InBool Unit Top)  = logInProducer "Choosing all done" >>
                            reads vConfig >>= getResult >>= store
choose (InBool l@Ref{} _) = logInProducer "Choosing all done" >>
                              evaluate l >>= removeChoices
choose loc =
  do
    let !loc' = findChoice loc
    case loc' of
      (InBool (Chc d cl cr) ctx) -> do
        conf <- reads config
        -- we'd like to evaluate after IL but this makes the async harder so we
        -- iAccumulate instead. Specifically there is an interaction with in new
        -- assertion stack. When requests come out of order the assertion stack
        -- scope is also out of order, because evaluation relies on this
        -- ordering we cannot use it.
        logInProducerWith "Choose Context" ctx
        -- let goLeft  = toIL cl >>= choose . findChoice . toLocWith ctx . iAccumulate
        --     goRight = toIL cr >>= choose . findChoice . toLocWith ctx . iAccumulate

        let goLeft  = toIL cl >>= evaluate . snd >>= choose . findChoice . toLocWith ctx . getCore
            goRight = toIL cr >>= evaluate . snd >>= choose . findChoice . toLocWith ctx . getCore

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
        let goLeft  = toIL' cl >>= choose . findChoice . toLocWith' ctx . snd . iAccumulate' . snd
            goRight = toIL' cr >>= choose . findChoice . toLocWith' ctx . snd . iAccumulate' . snd

        case find d conf of
          Just True  -> logInProducer "Cache hit --- Left Selected"  >> goLeft
          Just False -> logInProducer "Cache hit --- Right Selected" >> goRight
          Nothing    -> alternative d goLeft goRight

      x -> error $ "Choosing and missed cases with: " ++ show x


--------------------------- Variant Context Helpers ----------------------------
contextToSBool' :: ( Monad m
                   , Constrainable m Dim T.SBool
                   ) => VariantContext -> m T.SBool
contextToSBool' (getVarFormula -> x) = go x
  where -- go :: Show a => Prop' a -> m T.SBool
        go (LitB True)  = return T.sTrue
        go (LitB False) = return T.sFalse
        go (RefB d)     = cached d
        go (OpB Not e) = bnot <$> go e
        go (OpBB op l r) = do l' <- go l
                              r' <- go r
                              let op' = dispatch op
                              return $ l' `op'` r'
        go OpIB {} = error "numeric expressions are invalid in variant context"
        go ChcB {} = error "variational expressions are invalid in variant context"

        dispatch And  = (&&&)
        dispatch Or   = (|||)
        dispatch Impl = (==>)
        dispatch Eqv  = (<=>)
        dispatch XOr  = (<=>)

contextToSBool :: ( R.MonadReader State  m
                  , C.MonadQuery         m
                  , MonadIO              m
                  ) => VariantContext -> m T.SBool
contextToSBool (getVarFormula -> x) = go x
  where -- go :: Show a => Prop' a -> m T.SBool
        go (LitB True)  = return T.sTrue
        go (LitB False) = return T.sFalse
        go (RefB d)     = do dims <- reads dimensions
                             case Map.lookup d dims of
                               Just b -> return b
                               Nothing -> do newSym <- T.label (Text.unpack $ getDim d) <$> C.freshVar (Text.unpack $ getDim d)
                                             update dimensions (add d newSym)
                                             return newSym
        go (OpB Not e) = bnot <$> go e
        go (OpBB op l r) = do l' <- go l
                              r' <- go r
                              let op' = dispatch op
                              return $ l' `op'` r'
        go OpIB {} = error "numeric expressions are invalid in variant context"
        go ChcB {} = error "variational expressions are invalid in variant context"

        dispatch And  = (&&&)
        dispatch Or   = (|||)
        dispatch Impl = (==>)
        dispatch Eqv  = (<=>)
        dispatch XOr  = (<=>)

instance Pretty V where
  pretty P = "plain"
  pretty V = "var"

instance Pretty IL where
  pretty Unit = "unit"
  pretty (Ref b)      = pretty b
  pretty (BOp Not r@(_,Ref _))   = "~" <> pretty r
  pretty (BOp o e)   = pretty o <> parens (pretty e)
  pretty (BBOp op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (IBOp op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (Chc d l r)  = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"

instance Pretty NRef where
  pretty (SI _) = "sInt"
  pretty (SD _) = "sDouble"

instance Pretty IL' where
  pretty (Ref' b)     = pretty b
  pretty (IOp o x@(_,Ref' _))  = pretty o <> pretty x
  pretty (IOp Abs e)  = between "|" (pretty e) "|"
  pretty (IOp o e)    = pretty o <> parens (pretty e)
  pretty (IIOp o l r) = parens $ mconcat [pretty l, " ", pretty o, " ", pretty r]
  pretty (Chc' d l r) = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"
