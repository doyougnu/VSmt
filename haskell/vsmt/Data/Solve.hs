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
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Solve where

import qualified Control.Concurrent.Async              as A (async,cancel,mapConcurrently_)
import qualified Control.Concurrent.STM                as STM (modifyTVar', readTVarIO, newTVarIO, TVar, atomically,readTVar)
import qualified Control.Concurrent.Chan.Unagi.Bounded as U
import           Control.Monad                         (forever, void, when)
import           Control.Monad.Except                  (MonadError)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Logger                  (LoggingT,
                                                        MonadLogger(..),
                                                        NoLoggingT,
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
import qualified Data.IntMap.Strict                    as IMap
import           Data.Maybe                            (fromJust)
import           Data.Monoid                           (Sum(..))
import           Data.Hashable                         (Hashable)
import qualified Data.SBV.Internals                    as I
import qualified Data.SBV.Trans                        as T
import qualified Data.SBV.Trans.Control                as C

import qualified Data.Text                             as Text
import           GHC.Generics                          (Generic)
import           Control.DeepSeq                       (NFData)

import           Prelude                               hiding (EQ, GT, LT, log,
                                                        putStrLn,read,reads)

import           Core.Pretty
import           Core.Result
import           Core.Types
import           Core.Utils

import           Settings

--------------------------------- Specific logging ----------------------------
logInProducer :: (R.MonadReader State m, MonadLogger m) => Text.Text -> m ()
logInProducer msg = R.asks (threadId . channels) >>= flip logInProducer' msg

logInProducerWith :: (R.MonadReader State m, MonadLogger m, Show a) =>
  Text.Text -> a -> m ()
logInProducerWith msg value = do tid <- R.asks (threadId . channels)
                                 logInThreadWith "Producer" tid msg value

logInConsumer :: (R.MonadReader State m, MonadLogger m) => Text.Text -> m ()
logInConsumer msg = R.asks (threadId . channels) >>= flip logInConsumer' msg


------------------------------ Internal Api -------------------------------------
findVCore :: ( MonadLogger z3
             , Cacheable   z3 Tag (V :/\ IL)
             , MonadReader State z3
             , I.SolverContext z3
             ) => IL -> z3 VarCore
findVCore = evaluate

solveVerbose :: Maybe VariantContext -> Settings -> Proposition -> IO Result
solveVerbose v s p = sFst <$> internalSolver runPreSolverLog runSolverLog v s p

solve :: Maybe VariantContext -> Settings -> Prop' Var -> IO Result
solve v s p = sFst <$> internalSolver runPreSolverNoLog runSolverNoLog v s p

-- | the solve but return the diagnostics
solveGetDiag :: Maybe VariantContext -> Settings -> Prop' Var -> IO FrozenDiags
solveGetDiag vc s p = readDiagnostics . sSnd =<< internalSolver runPreSolverNoLog runSolverNoLog vc s p



-- TODO fix this horrendous type signature
internalSolver ::
  ( C.MonadQuery m
  , I.SolverContext m
  , T.MonadSymbolic m
  , MonadLogger m
  , MonadLogger f
  , MonadIO f
  , Constrainable f (ExRefType Var) IL'
  , Constrainable f Var IL
  , MonadReader State f
  ) => (State -> f IL -> T.SymbolicT IO (IL :/\ State))
  -> (State -> SolverT m Result -> C.Query b)
  -> Maybe VariantContext
  -> Settings
  -> Prop' Var
  -> IO b
internalSolver preSlvr slvr conf s@Settings{..} i = do
  (toMain, fromVC)   <- U.newChan vcBufSize
  (toVC,   fromMain) <- U.newChan vcBufSize
  initialStore       <- newStore
  initialCache       <- newCaches
  initialResults     <- newResults
  initialDiagnostics <- newDiagnostics
  initialConstants   <- newConstants s


  let il = toIL i
      solverConfig = unSolver solver

  -- init the channels
      vcChans   = VCChannels   (fromMain, toMain)
      mainChans = MainChannels (fromVC, toVC)
      chans     = Channels{ vcChans=vcChans, mainChans=mainChans, threadId=0}
      startState  = State{ stores      = initialStore
                         , channels    = chans
                         , caches      = initialCache
                         , results     = initialResults
                         , diagnostics = initialDiagnostics
                         , constants   = initialConstants
                         }
      seasoning = preSlvr startState (sSnd <$> il)

      runVCWorkers =
        A.mapConcurrently_ (vcWorker solverConfig conf startState slvr)
        [1..numVCWorkers]

      -- this thread will exit once it places requests on the producer
      -- chans. If the IL is a unit then it'll be caught by evaluate and
      -- placed on a result chan anyway
      populateChans = T.runSMTWith solverConfig $
        do (il' :/\ st) <- seasoning
           C.query $ slvr st $
             do void $ findVCore il' >>= removeChoices
                readWith (unResults . results)

  -- kick off
  !aWorkers  <- A.async runVCWorkers

  result <- populateChans

  A.cancel aWorkers

  return result

solveForCore :: Proposition -> IO (VarCore :/\ State)
solveForCore i = do
  (toMain, fromVC)   <- U.newChan 1
  (toVC,   fromMain) <- U.newChan 1
  initialStore       <- newStore
  initialCache       <- newCaches
  initialResults     <- newResults
  initialDiagnostics <- newDiagnostics
  initialConstants   <- newConstants defSettings{generateModels=False}

  let il           = toIL i
      solverConfig = unSolver . solver $ defSettings

      vcChans   = VCChannels   (fromMain, toMain)
      mainChans = MainChannels (fromVC, toVC)
      chans     = Channels{ vcChans=vcChans, mainChans=mainChans, threadId=0}
      startState  = State{ stores   = initialStore
                         , channels = chans
                         , caches   = initialCache
                         , results  = initialResults
                         , diagnostics = initialDiagnostics
                         , constants   = initialConstants
                         }
      seasoning = runPreSolverNoLog startState (sSnd <$> il)

      populateChans = T.runSMTWith solverConfig $
        do (il' :/\ st) <- seasoning
           C.query $
             runSolverNoLog st $ findVCore il'

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
  , C.MonadQuery f
  , MonadIO         f
  , I.SolverContext f
  , Constrainable   f Dim (T.SBV Bool)
  ) => T.SMTConfig -> Maybe VariantContext -> State -> (State -> f b -> C.Query a) -> Int -> IO ()
vcWorker c Nothing s@State{..} slvr tid =
  let (fromMain, toMain) = getVcChans . vcChans $ channels
  in T.runSMTWith c $ vcHelper fromMain toMain s slvr tid
vcWorker c (Just vc) s@State{..} slvr tid =
  T.runSMTWith c $ do
  -- season the solver
  let (fromMain, toMain) = getVcChans . vcChans $ channels
  (b :/\ st) <- runPreSolverLog s $ contextToSBool vc
  T.constrain b
  -- now run the helper
  vcHelper fromMain toMain st slvr tid

vcHelper :: (C.ExtractIO m, MonadLogger f, C.MonadQuery f,
             Constrainable f a1 T.SBool, I.SolverContext f, MonadIO f) =>
  U.OutChan (a1, Bool, T.SBV Bool)
  -> U.InChan (Bool, T.SBV Bool)
  -> t
  -> (t -> f b -> C.QueryT m a2)
  -> Int
  -> T.SymbolicT m ()
vcHelper fromMain toMain st slvr _ =
  -- listen to the channel forever and check requests incrementally against the
  -- context if there is one
  C.query $
    void $
    slvr st $
    forever $
     do
       -- logInVC' tid "Waiting"
       (d, shouldNegate, vc') <- liftIO $ U.readChan fromMain
       -- logInVC' tid "got request"
       C.inNewAssertionStack $
         do sD <- constrain d
            -- logInVCWith tid "before" sD
            let !new = if shouldNegate then bnot sD else sD &&& vc'
            -- logInVCWith tid "created" new
            T.constrain new
            C.checkSat >>= liftIO . \case
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
  { stores      :: !Stores
  , channels    :: !Channels
  , caches      :: !Caches
  , results     :: !Results
  , diagnostics :: !Diagnostics
  , constants   :: !Constants
  }

type Cache a = IMap.IntMap a
type ACache  = Cache (V :/\ IL)
type CtxCache = Cache Loc

newtype Results = Results { unResults :: STM.TVar Result }

-- | A counter for the diagnostics portion of the state
data Counter =  Counter {-# UNPACK #-} !Int
  deriving (Eq,Show,Generic)

instance Enum Counter where
  toEnum               = Counter
  fromEnum (Counter i) = i
instance Semigroup Counter where (<>) (Counter l) (Counter r) = Counter $! l + r
instance Monoid    Counter where mempty = Counter 0

unCounter :: Counter -> Int
unCounter = fromEnum

data Constants = Constants { genModels  :: STM.TVar Bool
                           }

data Diagnostics = Diagnostics
                 { satCnt       :: !(STM.TVar Counter)
                 , unSatCnt     :: !(STM.TVar Counter)
                 , accCacheHits :: !(STM.TVar Counter)
                 , accCacheMiss :: !(STM.TVar Counter)
                 }

-- TODO abstract frozen into a type class or type family?
data FrozenDiags = FrozenDiags
                   { fSatCnt       :: !Counter
                   , fUnSatCnt     :: !Counter
                   , fAccCacheHits :: !Counter
                   , fAccCacheMiss :: !Counter
                   } deriving (Generic, Show)

data FrozenCaches = FrozenCaches
                    { faccCache :: ACache
                    , fctxCache :: CtxCache
                    , faccTagSeed :: Tag
                    , fctxTagSeed :: Tag
                    }

data Caches = Caches
              { accCache :: STM.TVar ACache
              , ctxCache :: STM.TVar CtxCache
              , accTagSeed :: STM.TVar Tag
              , ctxTagSeed :: STM.TVar Tag
              }

data Stores = Stores
    { vConfig    :: STM.TVar (Maybe VariantContext)  -- the formula representation of the config
    , sConfig    :: STM.TVar SVariantContext -- symbolic representation of a config
    , config     :: STM.TVar Context                 -- a map or set representation of the config
    , ints       :: STM.TVar Ints
    , doubles    :: STM.TVar Doubles
    , bools      :: STM.TVar Bools
    , dimensions :: STM.TVar Dimensions
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
                         , threadId  :: ThreadID
                         }
newCaches :: IO Caches
newCaches = do accCache <- STM.newTVarIO mempty
               ctxCache <- STM.newTVarIO mempty
               accTagSeed <- STM.newTVarIO (Tag 0)
               ctxTagSeed <- STM.newTVarIO (Tag 0)
               return Caches{..}

newResults :: IO Results
newResults = Results <$> STM.newTVarIO mempty

newStore :: IO Stores
newStore = do vConfig    <- STM.newTVarIO mempty
              sConfig    <- STM.newTVarIO mempty
              config     <- STM.newTVarIO mempty
              ints       <- STM.newTVarIO mempty
              doubles    <- STM.newTVarIO mempty
              bools      <- STM.newTVarIO mempty
              dimensions <- STM.newTVarIO mempty
              return Stores{..}

newConstants :: Settings -> IO Constants
newConstants Settings{..} = do genModels  <- STM.newTVarIO generateModels
                               return Constants{..}

newDiagnostics :: IO Diagnostics
newDiagnostics = do satCnt       <- STM.newTVarIO mempty
                    unSatCnt     <- STM.newTVarIO mempty
                    accCacheHits <- STM.newTVarIO mempty
                    accCacheMiss <- STM.newTVarIO mempty
                    return Diagnostics{..}

readDiagnostics :: State -> IO FrozenDiags
readDiagnostics s = do !fSatCnt       <- fullRead (satCnt       . diagnostics) s
                       !fUnSatCnt     <- fullRead (unSatCnt     . diagnostics) s
                       !fAccCacheHits <- fullRead (accCacheHits . diagnostics) s
                       !fAccCacheMiss <- fullRead (accCacheMiss . diagnostics) s
                       return FrozenDiags{..}

update :: (R.MonadReader State io, MonadIO io) => (Stores -> STM.TVar a) -> (a -> a) -> io ()
update field = updateWith (field . stores)

reads :: (R.MonadReader State io, MonadIO io) => (Stores -> STM.TVar a) -> io a
reads f = readWith (f . stores)

readCache :: (R.MonadReader State io, MonadIO io) => (Caches -> STM.TVar a) -> io a
readCache f = readWith (f . caches)

updateCache :: (R.MonadReader State io, MonadIO io) => (Caches -> STM.TVar a) -> (a -> a) -> io ()
updateCache field = updateWith (field . caches)

succSatCnt :: (R.MonadReader State io, MonadIO io) => io ()
succSatCnt = updateWith (satCnt . diagnostics) succ

succUnSatCnt :: (R.MonadReader State io, MonadIO io) => io ()
succUnSatCnt = updateWith (unSatCnt . diagnostics) succ

succAccCacheHits :: (R.MonadReader State io, MonadIO io) => io ()
succAccCacheHits = updateWith (accCacheHits . diagnostics) succ

succAccCacheMiss :: (R.MonadReader State io, MonadIO io) => io ()
succAccCacheMiss = updateWith (accCacheMiss . diagnostics) succ

updateWith :: (R.MonadReader s io, MonadIO io) => (s -> STM.TVar a) -> (a -> a) -> io ()
updateWith field f = R.asks field >>=
                     liftIO . STM.atomically . flip STM.modifyTVar' f

readWith :: (R.MonadReader s io, MonadIO io) => (s -> STM.TVar a) -> io a
readWith f = R.asks f  >>= liftIO . STM.readTVarIO

read :: MonadIO io => (s -> STM.TVar a) -> s -> io a
read f = liftIO . STM.readTVarIO . f

fullRead :: MonadIO io => (s -> STM.TVar a) -> s -> io a
fullRead f = liftIO . STM.atomically . STM.readTVar . f

fromState :: MonadIO io => (State -> STM.TVar a) -> State -> io a
fromState f = liftIO . STM.readTVarIO . f

resultFromState :: State -> IO Result
resultFromState = fromState (unResults . results)

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

freezeCache :: (R.MonadReader State io, MonadIO io) => io FrozenCaches
freezeCache = do st          <- R.asks caches
                 faccCache   <- read accCache   st
                 fctxCache   <- read ctxCache   st
                 faccTagSeed <- read accTagSeed st
                 fctxTagSeed <- read ctxTagSeed st
                 return FrozenCaches{..}
-- | A solver is just a reader over a solver enabled monad. The reader
-- maintains information during the variational execution, such as
-- configuration, variable stores
newtype SolverT m a = SolverT { runSolverT :: R.ReaderT State m a }
  deriving newtype ( Functor,Applicative,Monad,MonadIO
                   , MonadError e, MonadLogger, R.MonadReader State
                   , MonadTrans, T.MonadSymbolic, C.MonadQuery
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
newtype PreSolverT m a = PreSolverT { runPreSolverT :: (R.ReaderT State m) a }
  deriving newtype ( Functor,Applicative,Monad,MonadIO
                   , MonadError e, MonadTrans, MonadLogger
                   , R.MonadReader State, T.MonadSymbolic
                   , C.MonadQuery
                   )

runPreSolverLog :: State -> PreSolverLog a -> T.Symbolic (a :/\ State)
runPreSolverLog s = fmap (:/\s) . runStdoutLoggingT . flip R.runReaderT s . runPreSolverT

runPreSolverNoLog :: State -> PreSolver a -> T.Symbolic (a :/\ State)
runPreSolverNoLog s = fmap (:/\s) . runNoLoggingT . flip R.runReaderT s . runPreSolverT

runSolverNoLog :: State -> Solver a -> C.Query (a :/\ State)
runSolverNoLog s = fmap (:/\s) . runNoLoggingT . flip R.runReaderT s . runSolverT

runSolverLog :: State -> SolverLog a -> C.Query (a :/\ State)
runSolverLog s = fmap (:/\s) . runStdoutLoggingT . flip R.runReaderT s . runSolverT

type PreRun m a = State -> m a -> T.Symbolic (a :/\ State)
type Run m    a = State  -> SolverT m a -> C.Query (a :/\ State)

class RunPreSolver s where
  runPreSolver :: State -> s a -> T.Symbolic (a :/\ State)

class RunSolver s where
  runSolver :: State -> s a -> C.Query (a :/\ State)

instance RunSolver SolverLog       where runSolver    = runSolverLog
instance RunSolver Solver          where runSolver    = runSolverNoLog
instance RunPreSolver PreSolverLog where runPreSolver = runPreSolverLog
instance RunPreSolver PreSolver    where runPreSolver = runPreSolverNoLog


class Show a => Constrainable m a b where constrain :: a -> m b

-- -- TODO fix this duplication with derivingVia
instance (Monad m, T.MonadSymbolic m, C.MonadQuery m, MonadLogger m) =>
  Constrainable (SolverT m) Var IL where
  constrain ref = do
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
  constrain d = do
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
  constrain (ExRefTypeI i) =
    do st <- reads ints
       case find i st of
         Just x  -> return . Ref' . SI $ x
         Nothing -> do newSym <- T.label (Text.unpack i) <$> C.freshVar (Text.unpack i)
                       update ints (add i newSym)
                       return (Ref' . SI $ newSym)

  constrain (ExRefTypeD d) =
    do st <- reads doubles
       case find d st of
         Just x  -> return . Ref' $ SD x
         Nothing -> do newSym <- T.label (Text.unpack d) <$> C.freshVar (Text.unpack d)
                       update doubles (add d newSym)
                       return $! Ref' $ SD newSym

instance (Monad m, T.MonadSymbolic m, MonadLogger m) =>
  Constrainable (PreSolverT m) Var IL where
  constrain ref   = do st <- reads bools
                       case find ref st of
                         Just x  -> return (Ref x)
                         Nothing -> do newSym <- T.sBool (Text.unpack ref)
                                       update bools (add ref newSym)
                                       return (Ref newSym)


instance (Monad m, T.MonadSymbolic m) =>
  Constrainable (PreSolverT m) (ExRefType Var) IL' where
  constrain (ExRefTypeI i) =
    do st <- reads ints
       case find i st of
         Just x  -> return . Ref' . SI $ x
         Nothing -> do newSym <- T.sInteger (Text.unpack i)
                       update ints (add i newSym)
                       return (Ref' . SI $ newSym)

  constrain (ExRefTypeD d) =
    do st <- reads doubles
       case find d st of
         Just x  -> return . Ref' $ SD x
         Nothing -> do newSym <- T.sDouble (Text.unpack d)
                       update doubles (add d newSym)
                       return $! Ref' $ SD newSym

instance (Monad m, T.MonadSymbolic m) =>
  Constrainable (PreSolverT m) Dim T.SBool where
  constrain d = do
    ds <- reads dimensions
    case find d ds of
      Just x -> return x
      Nothing -> do
        let ref = Text.unpack $ getDim d
        newSym <- T.sBool ref
        update dimensions (add d newSym)
        return newSym

-- | A general caching mechanism using StableNames. There is a small chance of a
-- collision ~32k per 2^24. I leave this completely unhandled as it is so rare I
-- doubt it'll ever actually occur
-- TODO use Type Families to abstract over the monad and cache
class Cacheable m a b where
  memo  :: a -> m b -> m b

instance (Monad m, MonadIO m, MonadLogger m) =>
  Cacheable (SolverT m) Tag (V :/\ IL) where
  memo (unTag -> a) go = do acc <- readCache accCache
                            case IMap.lookup a acc of
                              Just b -> do logInProducerWith "Acc Cache Hit on " a
                                           succAccCacheHits
                                           return b
                              Nothing -> do !b <- go
                                            logInProducerWith "Acc Cache miss on " a
                                            updateCache accCache $! IMap.insert a b
                                            succAccCacheMiss
                                            return b

instance (Monad m, MonadIO m, MonadLogger m) =>
  Cacheable (SolverT m) Tag Loc where
  memo (unTag -> a) go = do acc <- readCache ctxCache
                            case IMap.lookup a acc of
                              Just b -> do logInProducerWith "Acc Cache Hit on " a
                                           succAccCacheHits
                                           return b
                              Nothing -> do !b <- go
                                            logInProducerWith "Acc Cache miss on " a
                                            updateCache ctxCache $! IMap.insert a b
                                            succAccCacheMiss
                                            return b

----------------------------------- IL -----------------------------------------
type BRef = T.SBool

generateAccKey :: (MonadIO m, R.MonadReader State m) => m Tag
generateAccKey = updateWith (accTagSeed . caches) succ
                 >> readWith (accTagSeed . caches)

generateCtxKey :: (MonadIO m, R.MonadReader State m) => m Tag
generateCtxKey = updateWith (ctxTagSeed . caches) succ
                 >> readWith (ctxTagSeed . caches)

data NRef = SI T.SInteger
          | SD T.SDouble
    deriving stock (Generic,Show,Eq)

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

newtype Tag = Tag { unTag :: Int }
  deriving stock (Eq,Ord,Generic,Show)
  deriving (Enum, Num) via Int
  deriving anyclass (NFData,Hashable)
  deriving (Semigroup,Monoid) via (Sum Int)

-- | The intermediate language, we express negation but negation will not exist
-- in a variational core. The IL language is intermediate and used to collapse
-- the input program to it's essential variational structure. We exploit the
-- symbolic references to represent plain sub-trees such that the only thing
-- that will survive in the variational core are binary connectives, symbolic
-- references and choices
data IL = Unit
    | Ref !BRef
    | BOp  Tag  B_B (V :/\ IL)
    | BBOp Tag BB_B (V :/\ IL)  (V :/\ IL)
    | IBOp Tag NN_B (V :/\ IL') (V :/\ IL')
    | Chc Dim Proposition Proposition
    deriving stock (Generic, Show, Eq)

data IL' = Ref' !NRef
    | IOp  Tag  N_N  (V :/\ IL')
    | IIOp Tag  NN_N (V :/\ IL') (V :/\ IL')
    | Chc' Dim NExpression NExpression
    deriving stock (Generic, Show, Eq)

instance NFData IL
instance NFData IL'
instance NFData V
instance NFData NRef

-- | tags which describes where in the tree there is variation
data V = P | V deriving (Generic, Show, Eq, Hashable,Ord)

-- | property of infection
(<@>) :: V -> V -> V
V <@> V = V
V <@> P = V
P <@> V = V
P <@> P = P

-- | get the topmost variational indicator of the IL ast
getVofIL :: IL -> V
getVofIL (BOp  _ _ (v :/\ _))            = v
getVofIL (BBOp _ _ (l :/\ _) (r :/\ _)) = l <@> r
getVofIL (IBOp _ _ (l :/\ _) (r :/\ _)) = l <@> r
getVofIL _                            = P

getVofIL' :: IL' -> V
getVofIL' (IOp  _ _ (v :/\ _))            = v
getVofIL' (IIOp _ _ (l :/\ _) (r :/\ _)) = l <@> r
getVofIL' _                            = P

vCoreSize :: IL -> Int
vCoreSize (BBOp _ _ (_ :/\ l) (_:/\ r)) = vCoreSize l + vCoreSize r
vCoreSize (BOp  _ _ (_ :/\ e))           = vCoreSize e
vCoreSize (IBOp _ _ (_:/\ l) (_ :/\ r)) = vCoreSize' l + vCoreSize' r
vCoreSize _            = 1

vCoreSize' :: IL' -> Int
vCoreSize' (IOp  _ _ (_ :/\ e))    = vCoreSize' e
vCoreSize' (IIOp _ _ (_ :/\ l) (_ :/\ r)) = vCoreSize' l + vCoreSize' r
vCoreSize' _            = 1

vCoreNumPlain :: IL -> Int
vCoreNumPlain Chc {}                         = 0
vCoreNumPlain Unit                           = 0
vCoreNumPlain (BOp  _ _ (_ :/\ e))           = vCoreNumPlain e
vCoreNumPlain (BBOp _ _ (_ :/\ l) (_ :/\ r)) = vCoreNumPlain  l + vCoreNumPlain r
vCoreNumPlain (IBOp _ _ (_ :/\ l) (_ :/\ r)) = vCoreNumPlain' l + vCoreNumPlain' r
vCoreNumPlain Ref {}                         = 1

vCoreNumPlain' :: IL' -> Int
vCoreNumPlain' Chc' {}                        = 0
vCoreNumPlain' (IOp  _ _ (_ :/\ e))           = vCoreNumPlain' e
vCoreNumPlain' (IIOp _ _ (_ :/\ l) (_ :/\ r)) = vCoreNumPlain' l + vCoreNumPlain' r
vCoreNumPlain' _                              = 1

vCoreNumVar :: IL -> Int
vCoreNumVar Chc {} = 1
vCoreNumVar (BOp  _ _ (_ :/\ e)) = vCoreNumVar e
vCoreNumVar (BBOp _ _ (_ :/\ l) (_ :/\ r)) = vCoreNumVar  l + vCoreNumVar r
vCoreNumVar (IBOp _ _ (_ :/\ l) (_ :/\ r)) = vCoreNumVar' l + vCoreNumVar' r
vCoreNumVar Ref {} = 0
vCoreNumVar Unit   = 0

vCoreNumVar' :: IL' -> Int
vCoreNumVar' Chc' {} = 0
vCoreNumVar' (IOp  _ _ (_ :/\ e))    = vCoreNumVar' e
vCoreNumVar' (IIOp _ _ (_ :/\ l) (_ :/\ r)) = vCoreNumVar' l + vCoreNumVar' r
vCoreNumVar'  _      = 1

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
  ( MonadLogger      m
  , MonadIO          m
  , Constrainable    m (ExRefType Var) IL'
  , Constrainable    m Var IL
  , R.MonadReader State m
  ) => Prop' Var -> m (V :/\ IL)
toIL (LitB True)  = return . (P :/\) . Ref $ T.sTrue
toIL (LitB False) = return . (P :/\) . Ref $ T.sFalse
toIL (RefB ref)   = (P :/\) <$> constrain ref
toIL (OpB op e)      = do (v :/\ e') <- toIL e
                          k <- generateAccKey
                          return (v :/\  BOp k op (v :/\ e'))
toIL (OpBB op l r) = do l'@(vl :/\ _) <- toIL l
                        r'@(vr :/\ _) <- toIL r
                        k <- generateAccKey
                        return (vl <@> vr :/\ BBOp k op l' r')
toIL (OpIB op l r) = do l'@(vl :/\ _) <- toIL' l
                        r'@(vr :/\ _) <- toIL' r
                        k <- generateAccKey
                        return (vl <@> vr :/\ IBOp k op l' r')
toIL (ChcB d l r)  = return (V :/\ Chc d l r)

toIL' :: ( Constrainable m (ExRefType Var) IL'
         , MonadLogger   m
         , MonadIO       m
         , R.MonadReader State m
         ) =>
         NExpr' Var -> m (V :/\ IL')
toIL' (LitI (I i))  = return . (P :/\ ) . Ref' . SI $ T.literal i
toIL' (LitI (D d))  = return . (P :/\ ) . Ref' . SD $ T.literal d
toIL' (RefI a)      = (P :/\ ) <$> constrain a
toIL' (OpI op e)    = do e'@(v :/\ _)  <- toIL' e
                         k  <- generateAccKey
                         return (v :/\  IOp k op e')
toIL' (OpII op l r) = do l'@(vl :/\ _) <- toIL' l
                         r'@(vr :/\ _) <- toIL' r
                         k <- generateAccKey
                         return (vl <@> vr :/\ IIOp k op l' r')
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

dispatchOp :: Boolean b => BB_B -> b -> b -> b
{-# INLINE dispatchOp #-}
{-# SPECIALIZE dispatchOp :: BB_B -> T.SBool -> T.SBool -> T.SBool #-}
dispatchOp And  = (&&&)
dispatchOp Or   = (|||)
dispatchOp Impl = (==>)
dispatchOp Eqv  = (<=>)
dispatchOp XOr  = (<+>)

dispatchUOp' :: Num a => N_N -> a -> a
{-# INLINE dispatchUOp' #-}
dispatchUOp' Neg  = negate
dispatchUOp' Abs  = abs
dispatchUOp' Sign = signum

dispatchIOp' :: PrimN a => NN_N -> a -> a -> a
{-# INLINE dispatchIOp' #-}
dispatchIOp' Add  = (+)
dispatchIOp' Sub  = (-)
dispatchIOp' Div  = (./)
dispatchIOp' Mult = (*)
dispatchIOp' Mod  = (.%)

dispatchOp' :: T.OrdSymbolic a => NN_B -> a -> a -> T.SBool
{-# INLINE dispatchOp' #-}
dispatchOp' LT  = (T..<)
dispatchOp' LTE = (T..<=)
dispatchOp' EQ  = (T..==)
dispatchOp' NEQ = (T../=)
dispatchOp' GT  = (T..> )
dispatchOp' GTE = (T..>=)

-- | Unmemoized internal Accumulation: we purposefully are verbose to provide
-- the optimizer better opportunities. Accumulation seeks to combine as much as
-- possible the plain terms in the AST into symbolic references
accumulate :: ( Cacheable z3 Tag (V :/\ IL)
              , MonadReader State z3
              , MonadLogger z3
              ) => IL -> z3 (V :/\ IL)
{-# INLINE accumulate #-}
{-# SPECIALIZE accumulate :: IL -> Solver (V :/\ IL) #-}
 -- computation rules
accumulate Unit    = return (P :/\ Unit)
accumulate x@Ref{} = return (P :/\ x)
accumulate x@Chc{} = return (V :/\ x)
  -- bools
accumulate (BOp _ Not (_ :/\ Ref r))  = -- memo t $!
  return $! (P :/\ ) . Ref $! bnot r
accumulate (BBOp _ op (_ :/\ Ref l) (_ :/\ Ref r)) = -- memo t $!
  return $! (P :/\ ) . Ref $! dispatchOp op l r
  -- numerics
accumulate (IBOp _ op (_ :/\ Ref' l) (_ :/\ Ref' r)) = -- memo t $!
  return $! (P :/\ ) . Ref $! dispatchOp' op l r
  -- choices
accumulate x@(BBOp _ _ (_ :/\ Chc {})  (_ :/\ Chc {}))  = return (V :/\ x)
accumulate x@(BBOp _ _ (_ :/\ Ref _)   (_ :/\ Chc {}))  = return (V :/\ x)
accumulate x@(BBOp _ _ (_ :/\ Chc {})  (_ :/\ Ref _))   = return (V :/\ x)
accumulate x@(IBOp _ _ (_ :/\ Chc' {}) (_ :/\ Chc' {})) = return (V :/\ x)
accumulate x@(IBOp _ _ (_ :/\ Ref' _)  (_ :/\ Chc' {})) = return (V :/\ x)
accumulate x@(IBOp _ _ (_ :/\ Chc' {}) (_ :/\ Ref' _))  = return (V :/\ x)
 -- congruence rules
accumulate (BOp t Not (P :/\ e)) = -- memo t $!
  do (_ :/\ e') <- accumulate e
     let !res = BOp t Not (P :/\ e')
     accumulate res

accumulate (BOp t Not (V :/\ e)) =  -- memo t $!
  do (_ :/\ e') <- accumulate e
     let !res = BOp t Not (V :/\ e')
     return (V :/\ res)

accumulate (BBOp t op (P :/\ l) (P :/\ r)) = --  memo t $!
  do (_ :/\ l') <- accumulate l
     (_ :/\ r') <- accumulate r
     let !res = BBOp t op (P :/\ l') (P :/\ r')
     logInProducerWith "accumulating two refs: " res
     accumulate res

accumulate (BBOp t op (_ :/\ l) (_ :/\ r)) =  -- memo t $!
  do (vl :/\ l') <- accumulate l
     (vr :/\ r') <- accumulate r
     let !res  = BBOp t op (vl :/\ l') (vr :/\ r')
     return (vl <@> vr :/\ res)

accumulate (IBOp t op (P :/\ l) (P :/\ r)) =  -- memo t $!
  do (_ :/\ l') <- iAccumulate' l
     (_ :/\ r') <- iAccumulate' r
     let !res = IBOp t op (P :/\ l') (P :/\ r')
     accumulate res

accumulate (IBOp t op (_ :/\ l) (_ :/\ r)) =  -- memo t $!
  do a@(vl :/\ _) <- iAccumulate' l
     b@(vr :/\ _) <- iAccumulate' r
     let !res = IBOp t op a b
     return (vl <@> vr :/\  res)

iAccumulate' :: ( MonadReader State z3
                ) => IL' -> z3 (V :/\ IL')
  -- computation rules
iAccumulate' x@(Ref' _)                               = return (P :/\  x)
iAccumulate' (IOp _ op (_ :/\ Ref' n))                  = return $! (P :/\ ) . Ref' $! dispatchUOp' op n
iAccumulate' (IIOp _ op (_ :/\ Ref' l) (_ :/\ Ref' r))  = return $! (P :/\) . Ref' $! dispatchIOp' op l r
  -- choices
iAccumulate' x@Chc' {}                                    = return (V :/\ x)
iAccumulate' x@(IIOp _ _ (_ :/\ Chc' {}) (_ :/\ Chc' {})) = return (V :/\ x)
iAccumulate' x@(IIOp _ _ (_ :/\ Ref' _)  (_ :/\ Chc' {})) = return (V :/\ x)
iAccumulate' x@(IIOp _ _ (_ :/\ Chc' {}) (_ :/\ Ref' _))  = return (V :/\ x)
iAccumulate' (IIOp k op c@(_ :/\ Chc'{}) (P :/\ r)) = do !r' <- iAccumulate' r
                                                         return (V :/\ IIOp k op c r')
iAccumulate' (IIOp k op (P :/\ l) c@(_ :/\ Chc'{})) = do !l' <- iAccumulate' l
                                                         return (V :/\ IIOp k op l' c)
  -- congruence rules
iAccumulate' (IIOp k o (P :/\ l) (P :/\ r)) = do !x <- iAccumulate' l
                                                 !y <- iAccumulate' r
                                                 let !res = IIOp k o x y
                                                 iAccumulate' res

iAccumulate' (IOp k o (P :/\  e))  = do !e' <- iAccumulate' e
                                        let !res = IOp k o e'
                                        iAccumulate' res

iAccumulate' (IOp k o (_ :/\  e))  = do (v :/\ e') <- iAccumulate' e
                                        let res = IOp k o (v :/\ e')
                                        return (v :/\ res)

iAccumulate' (IIOp k o (_ :/\ l) (_ :/\ r)) = do x@(vl :/\ _) <- iAccumulate' l
                                                 y@(vr :/\ _) <- iAccumulate' r
                                                 let !res = IIOp k o x y
                                                 return (vl <@> vr :/\  res)

-------------------------------- Evaluation -----------------------------------
toSolver ::
  ( Monad               m
  , R.MonadReader State m
  , I.SolverContext     m
  , MonadLogger         m
  ) => T.SBool -> m VarCore
{-# INLINE toSolver #-}
toSolver a = do T.constrain a
                logInProducerWith "Solver knows about: " a
                return $! intoCore Unit

-- | Evaluation will remove plain terms when legal to do so, "sending" these
-- terms to the solver, replacing them to Unit to reduce the size of the
-- variational core
  -- computation rules
evaluate :: ( MonadLogger z3
            , I.SolverContext z3
            , MonadReader State z3
            , Cacheable   z3 Tag (V :/\ IL)
            ) => IL -> z3 VarCore
{-# INLINE evaluate #-}
{-# SPECIALIZE evaluate :: IL -> Solver VarCore #-}
evaluate Unit     = return $! intoCore Unit
evaluate (Ref b)  = toSolver b
evaluate x@Chc {} = return $! intoCore x
  -- bools
evaluate (BOp _ Not (_ :/\ Ref r))                  = toSolver $! bnot r
evaluate (BBOp _ op (_ :/\ Ref l) (_ :/\ Ref r))    = toSolver $! dispatchOp op l r
  -- numerics
evaluate (IBOp _ op  (_ :/\ Ref' l) (_ :/\ Ref' r)) = toSolver $! dispatchOp' op l r
  -- choices
evaluate x@(BBOp _ _ (V :/\ _) (V :/\ _)) = return $! intoCore x
evaluate x@(IBOp _ _ (V :/\ _) (V :/\ _)) = return $! intoCore x
  -- congruence cases
evaluate (BBOp _ And (_ :/\ l) (_ :/\ Unit))      = evaluate l
evaluate (BBOp _ And (_ :/\ Unit) (_ :/\ r))      = evaluate r
evaluate (BBOp _ And (_ :/\ l) (_ :/\ x@(Ref _))) = do _ <- evaluate x; evaluate l
evaluate (BBOp _ And (_ :/\ x@(Ref _)) (_ :/\ r)) = do _ <- evaluate x; evaluate r
evaluate (IBOp k op (P :/\ l) (P :/\ r))          = do !l' <- iAccumulate' l
                                                       !r' <- iAccumulate' r
                                                       let !res = IBOp k op l' r'
                                                       evaluate res

evaluate (IBOp k op (_ :/\ l) (_ :/\ r))          = do l' <- iAccumulate' l
                                                       r' <- iAccumulate' r
                                                       let res = IBOp k op l' r'
                                                       return $! intoCore res

  -- accumulation cases
evaluate x@(BOp _ Not (P :/\ _))  = accumulate x >>= evaluate . sSnd
evaluate x@(BOp _ Not (V :/\ _))  = intoCore . sSnd <$> accumulate x
evaluate (BBOp k And (P :/\ l) (P :/\ r)) = log "[Eval P P] And case" >>
  do (VarCore l') <- evaluate l
     (VarCore r') <- evaluate r
     let !res = BBOp k And (P :/\ l') (P :/\ r')
     evaluate res
evaluate (BBOp k And (V :/\ l) (P :/\ r)) = log "[Eval V P] And case" >>
  do (VarCore r') <- evaluate r
     let !res = BBOp k And (V :/\ l) (P :/\ r')
     evaluate res
evaluate (BBOp k And (P :/\ l) (V :/\ r)) = log "[Eval P V] And case" >>
  do (VarCore l') <- evaluate l
     let !res = BBOp k And (P :/\ l') (V :/\ r)
     evaluate res
evaluate (BBOp k op (P :/\ l) (P :/\ r)) = log "[Eval P P] General Case" >>
  do (_ :/\ l') <- accumulate l
     (_ :/\ r') <- accumulate r
     let !res = BBOp k op (P :/\ l') (P :/\ r')
     evaluate res
evaluate (BBOp k op (V :/\ l) (P :/\ r)) = log "[Eval V P] General Case" >>
  do (_ :/\ r') <- accumulate r
     let !res = BBOp k op (V :/\ l) (P :/\ r')
     return $! intoCore res
evaluate (BBOp k op (P :/\ l) (V :/\ r)) = log "[Eval P V] General Case" >>
  do (_ :/\ l') <- accumulate l
     let !res = BBOp k op (P :/\ l') (V :/\ r)
     return $! intoCore res

------------------------- Removing Choices -------------------------------------
-- TODO transform to a GADT
-- | We use a zipper to track the context when searching for choices, this
-- removes the need to perform tree rotations. We make this as strict as
-- possible because we know we will be consuming the entire structure so there
-- is not need to build thunks
data Ctx = InL  !Ctx   BB_B  IL
         | InR  !T.SBool BB_B  !Ctx
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

findChoice :: ( Cacheable z3 Tag (V :/\ IL)
              , MonadLogger z3
              , MonadReader State z3
              ) => Loc -> z3 Loc
  -- base cases
findChoice x@(InBool Ref{} Top) = return x
findChoice x@(InBool Unit Top)  = return x
findChoice x@(InBool Chc {} _)  = return x
findChoice x@(InNum Chc' {} _)  = return x
  -- discharge two references
findChoice x@(InBool l@Ref {} (InL parent op r@Ref {}))   =
  do (_ :/\ n) <- accumulate $ BBOp mempty op (P :/\ l) (P :/\ r)
     logInProducerWith "findChoice BRef: " x
     logInProducerWith "findChoice BRef constructed to: " $ BBOp mempty op (P :/\ l) (P :/\ r)
     logInProducerWith "findChoice BRef accumulated to: " n
     findChoice (InBool n parent)

findChoice (InNum l@Ref' {} (InLB parent op r@Ref' {})) =
  do (_ :/\ n) <- accumulate  $ IBOp mempty op (P :/\ l) (P :/\ r)
     findChoice (InBool n parent)

findChoice (InNum l@Ref' {} (InL' parent op r@Ref' {})) =
  do (_ :/\ n) <- iAccumulate' $ IIOp mempty op (P :/\ l) (P :/\ r)
     findChoice (InNum n parent)

  -- folds
findChoice (InBool r@Ref{} (InU o e))   =
  do (_ :/\ n) <- accumulate $! BOp mempty o (P :/\ r)
     findChoice (InBool n e)
findChoice (InNum r@Ref'{}  (InU' o e)) =
  do (_ :/\ n) <- iAccumulate' $! IOp mempty o (P :/\ r)
     findChoice (InNum n e)

findChoice (InBool r@Ref {} (InR acc op parent)) =
  do (_ :/\ n) <- accumulate $! BBOp mempty op (P :/\ Ref acc) (P :/\ r)
     findChoice (InBool n parent)

findChoice (InNum r@Ref' {} (InRB acc op parent)) =
  do (_ :/\ n) <- accumulate $! IBOp mempty op (P :/\ Ref' acc) (P :/\ r)
     findChoice (InBool n parent)

findChoice (InNum r@Ref' {} (InR' acc op parent)) =
  do (_ :/\ n) <- iAccumulate' $! IIOp mempty op (P :/\ Ref' acc) (P :/\ r)
     findChoice (InNum n parent)
  -- switch
findChoice (InBool (Ref l) (InL parent op r))   = findChoice (InBool r $ InR l op parent)
findChoice (InNum  (Ref' l) (InLB parent op r)) = findChoice (InNum  r $ InRB l op parent)
findChoice (InNum  (Ref' l) (InL' parent op r)) = findChoice (InNum  r $ InR' l op parent)
  -- recur
findChoice (InBool (BBOp _ op l@(P :/\ _) r@(P :/\ _)) ctx) =
  do (_ :/\ a) <- accumulate $! BBOp mempty op l r
     return (InBool a ctx)
findChoice (InBool (BBOp _ op (V :/\ l) (P :/\ r)) ctx) = findChoice (InBool l $ InL ctx op r)
findChoice (InBool (BBOp _ op (P :/\ l) (V :/\ r)) ctx) = do
  (_ :/\ new) <- accumulate l
  let (Ref l') = new
  findChoice (InBool r $ InR l' op ctx)
findChoice (InBool (BBOp _ op (V :/\ l) (V :/\ r)) ctx) = findChoice (InBool l $ InL ctx op r)
findChoice (InBool (IBOp _ op (_ :/\ l) (_ :/\ r)) ctx) = findChoice (InNum  l $ InLB ctx op r)
findChoice (InBool (BOp  _ o  (_ :/\ e))           ctx) = findChoice (InBool e $ InU o ctx)
findChoice (InNum  (IOp  _ o  (_ :/\ e))           ctx) = findChoice (InNum  e $ InU' o ctx)
findChoice (InNum  (IIOp _ op (_ :/\ l) (_ :/\ r)) ctx) = findChoice (InNum  l $ InL' ctx op r)
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
  , MonadLogger         io
  ) => Result -> io ()
{-# INLINE store #-}
{-# SPECIALIZE store :: Result -> Solver () #-}
store r = do
  logInProducerWith "Storing result: " r
  asks (unResults . results)
    >>= liftIO . STM.atomically . flip STM.modifyTVar' (r <>)

-- | TODO newtype this maybe stuff, this is an alternative instance
mergeVC :: Maybe VariantContext -> Maybe VariantContext -> Maybe VariantContext
{-# INLINE mergeVC #-}
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

resetCache :: (R.MonadReader State io, MonadIO io) => FrozenCaches -> io ()
{-# INLINE     resetCache #-}
{-# SPECIALIZE resetCache :: FrozenCaches -> Solver () #-}
resetCache FrozenCaches{..} = do updateCache accCache (const faccCache)
                                 updateCache ctxCache (const fctxCache)
                                 updateCache accTagSeed (const faccTagSeed)
                                 updateCache ctxTagSeed (const fctxTagSeed)

-- | Given a dimensions and a way to continue with the left alternative, and a
-- way to continue with the right alternative. Spawn two new subprocesses that
-- process the alternatives plugging the choice hole with its respective
alternative ::
  ( MonadIO      n
  , MonadLogger  n
  , C.MonadQuery n
  ) => Dim -> SolverT n () -> SolverT n () -> SolverT n ()
alternative dim goLeft goRight =
  do !s <- freeze
     -- c <- freezeCache
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
       let !continueLeft = C.inNewAssertionStack $
                           do logInProducerWith "Left Alternative of" dim
                              resetTo s

                              updateConfigs (bRef dim) (dim,True) newSConfigL
                              goLeft
       logInProducer "Writing to continue left"
       continueLeft

     -- resetTo s

     -- right side, notice that we negate the symbolic, and reset the state
     (checkDimFalse,!newSConfigR) <- liftIO $
       U.writeChan toVC (dim, pleaseNegate, symbolicContext) >> U.readChan fromVC
     when checkDimFalse $ do
       let !continueRight = C.inNewAssertionStack $
                            do logInProducerWith "Right Alternative of" dim
                               resetTo s
                               -- resetCache c
                               updateConfigs (bnot $ bRef dim) (dim,False) newSConfigR
                               goRight
       continueRight

removeChoices ::
  ( MonadLogger     m
  , MonadIO         m
  , C.MonadQuery    m
  , I.SolverContext m
  , T.MonadSymbolic m
  ) => VarCore -> SolverT m ()
removeChoices (VarCore Unit) = do !vC <- reads vConfig
                                  wantModels <- readWith (genModels . constants)
                                  (b :/\ r) <- if wantModels
                                               then getResult vC
                                               else isSat     vC
                                  reads config >>= logInProducerWith "Core reduced to Unit with Context"
                                  if b then succSatCnt else succUnSatCnt
                                  store r
removeChoices (VarCore x@(Ref _)) = do _ <- evaluate x; removeChoices (VarCore Unit)
removeChoices (VarCore l) = findChoice (toLoc l) >>= choose

choose ::
  ( MonadLogger       m
  , MonadIO           m
  , C.MonadQuery      m
  , T.MonadSymbolic   m
  , I.SolverContext   m
  ) => Loc -> SolverT m ()
choose (InBool Unit Top)  = removeChoices (VarCore Unit)
choose (InBool l@Ref{} _) = logInProducer "Choosing all done" >>
                            evaluate l >>= removeChoices
choose loc =
  do
    -- loc' <- findChoice loc
    case loc of
      (InBool (Chc d cl cr) ctx) -> do
        conf <- reads config
        -- we'd like to evaluate after IL but this makes the async harder so we
        -- accumulate instead. Specifically there is an interaction with in new
        -- assertion stack. When requests come out of order the assertion stack
        -- scope is also out of order, because evaluation relies on this
        -- ordering we cannot use it.
        let goLeft  = toIL cl >>= accumulate . sSnd >>= findChoice . toLocWith ctx . sSnd >>= choose
            goRight = toIL cr >>= accumulate . sSnd >>= findChoice . toLocWith ctx . sSnd >>= choose

        case find d conf of
          Just True  -> goLeft
          Just False -> goRight
          Nothing    -> alternative d goLeft goRight

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
                   , Constrainable m Dim T.SBool
                   ) => VariantContext -> m T.SBool
contextToSBool (getVarFormula -> x) = go x
  where -- go :: Show a => Prop' a -> m SDimension
        go (LitB True)  = return T.sTrue
        go (LitB False) = return T.sFalse
        go (RefB d)     = constrain d
        go (OpB Not e) = bnot <$> go e
        go (OpBB op l r) = do l' <- go l
                              r' <- go r
                              let op' = dispatchOp op
                              return $! l' `op'` r'
        go OpIB {} = error "numeric expressions are invalid in variant context"
        go ChcB {} = error "variational expressions are invalid in variant context"


instance Pretty V where
  pretty P = "plain"
  pretty V = "var"

instance (Pretty a, Pretty b) => Pretty (a :/\ b) where
  pretty (a :/\ b) = "(" <> pretty a <> "," <> pretty b <> ")"

instance Pretty IL where
  pretty Unit = "unit"
  pretty (Ref b)      = pretty b
  pretty (BOp _ Not r@(_ :/\ Ref _))   = "~" <> pretty r
  pretty (BOp _ o e)   = pretty o <> parens (pretty e)
  pretty (BBOp _ op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (IBOp _ op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (Chc d l r)  = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"

instance Pretty NRef where
  pretty (SI _) = "sInt"
  pretty (SD _) = "sDouble"

instance Pretty IL' where
  pretty (Ref' b)     = pretty b
  pretty (IOp _ o x@(_ :/\ Ref' _))  = pretty o <> pretty x
  pretty (IOp _ Abs e)  = between "|" (pretty e) "|"
  pretty (IOp _ o e)    = pretty o <> parens (pretty e)
  pretty (IIOp _ o l r) = parens $ mconcat [pretty l, " ", pretty o, " ", pretty r]
  pretty (Chc' d l r) = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"
