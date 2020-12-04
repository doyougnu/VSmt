-----------------------------------------------------------------------------
-- |
-- Module    : Data.Solve
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module that solves a variational smt problem
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.Solve where

import           Control.Applicative                   ((<|>))
import           Control.Concurrent                    (forkIO, killThread)
import qualified Control.Concurrent.Async              as A
import qualified Control.Concurrent.Chan.Unagi.Bounded as U
import           Control.Monad                         (forever, void, when)
import           Control.Monad.Except                  (MonadError)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Logger                  (LoggingT,
                                                        MonadLogger (..),
                                                        NoLoggingT, logDebug,
                                                        runStdoutLoggingT)
import qualified Control.Monad.State.Strict            as St (MonadState,
                                                              StateT, get, gets,
                                                              modify', put,
                                                              runStateT)
import           Control.Monad.Trans                   (MonadIO, MonadTrans,
                                                        lift)
import qualified Data.HashMap.Strict                   as Map
import           Data.Maybe                            (fromJust, fromMaybe)
import qualified Data.SBV.Internals                    as I
import qualified Data.SBV.Trans                        as T
import qualified Data.SBV.Trans.Control                as C
import qualified Data.Text                             as Text
import           Prelude                               hiding (EQ, GT, LT, log)

import           Data.Core.Core                        (posVariantCnt)
import           Data.Core.Pretty
import           Data.Core.Result
import           Data.Core.Types

import           Data.Settings

------------------------------ Template Haskell --------------------------------
log :: MonadLogger m => Text.Text -> m ()
log = $(logDebug)

-- TODO custom loggers for each function, i.e., [DEBUG:EVAL]: ...
logWith :: (MonadLogger m, Show a) => Text.Text -> a -> m ()
logWith msg value = log $ msg <> sep <> Text.pack (show value)
  where sep :: Text.Text
        sep = " : "

logPretty :: (Pretty a, MonadLogger m, Show a) => Text.Text -> a -> m ()
logPretty msg value = log $ msg <> sep <> pretty value
  where sep :: Text.Text
        sep = " : "

logState :: Solver ()
logState = St.get >>= logWith "State: "


logInThread :: MonadLogger m => Int -> Text.Text -> m ()
logInThread tid msg = log logmsg
  where
    logmsg :: Text.Text
    logmsg = "[Thread: " <> Text.pack (show tid) <> "] " <> "==> " <> msg

logInThreadWith :: (MonadLogger m, Show a) => Int -> Text.Text -> a -> m ()
logInThreadWith tid msg = logWith logmsg
  where
    logmsg :: Text.Text
    logmsg = "[Thread: " <> Text.pack (show tid) <> "] " <> "==> " <> msg

logThenPass :: (MonadLogger m, Show b) => Text.Text -> b -> m b
logThenPass str a = logWith str a >> return a

logThreadPass :: (MonadLogger m, Show b) => Int -> Text.Text -> b -> m b
logThreadPass tid str a = logInThreadWith tid str a >> return a

------------------------------ Internal Api -------------------------------------
findVCore :: IL -> Solver VarCore
findVCore = evaluate

solution :: (St.MonadState State m, Has Result) => m Result
solution = extract <$> St.get

-- | TODO pending on server create, create a load function to handle injection
-- | TODO reduce redundancy after config module is written
-- to the IL type
solveVerbose :: Proposition -> Maybe VariantContext -> Settings -> IO Result
solveVerbose  i (fromMaybe true -> conf) Settings{..} =
  do (toMain, fromVC)             <- U.newChan vcBufSize
     (toVC,   fromMain)           <- U.newChan vcBufSize
     (wcRequest, wcResponse)      <- U.newChan producerBufSize
     (resultsIn, finishedResults) <- U.newChan producerBufSize

  -- init the channels
     let vcChans     = VCChannels     $ pure (fromMain, toMain)
         mainChans   = MainChannels   $ pure (fromVC, toVC)
         workChans   = WorkChannels   $ pure (wcRequest, wcResponse)
         resultChans = ResultChannels $ pure (resultsIn, finishedResults)
         startState  = mempty{ vcChans=vcChans, mainChans=mainChans
                             , workChans=workChans, resultChans=resultChans}

         -- solver settings
         posVariants  = posVariantCnt i
         solverConfig = getConfig solver

         -- This is our "main" thread, we're going to reduce to a variational
         -- core and spawn producers. The producers will block on a work channel
         -- while this thread continues to solve, thereby populating the work
         -- channel.
         runProducers =
           T.runSMTWith solverConfig $ do
             C.io $ putStrLn "preconditioning in main"
             (il, st) <- runSolverWith runStdoutLoggingT startState $ unPre $ toIL i
             -- TODO load the state explicitly after exposing instance in Data.SBV.Trans
             -- seasoning <- T.symbolicEnv
             let seasoning = runSolverWith runStdoutLoggingT startState $ unPre $ toIL i
             -- spawn producers, we fork a thread to spawn producers here to
             -- prevent this call from blocking. The default behavior of
             -- mapConcurrently is to wait for results. We only know that a max
             -- of 2^(# unique dimensions) is the max, but the user may only
             -- want a subset, thus we can't wait for them to finish.
             _ <- liftIO $ forkIO $ A.mapConcurrently_ (producer seasoning solverConfig st) [1..numProducers]
             -- now continue to solver thereby placing work on the channel
             C.query $
               runSolverWith runStdoutLoggingT st $
               findVCore il >>= removeChoices

         accumulateResults !acc !n = if n == fromMaybe posVariants numResults
                                     then return acc
                                     else do r <- U.readChan finishedResults
                                             when verboseMode . putStrLn $ "[Accumulator] got result; n is: " ++ show n
                                             accumulateResults (r <> acc) (succ n)

     tid <- forkIO $ initVCWorker conf vcChans

     -- kick off
     (_,results)<- runProducers `A.concurrently` accumulateResults mempty 0
     killThread tid -- if we get here we're done
     return results

-- TODO runNoLogging
solve :: Proposition -> Maybe VariantContext -> Settings -> IO Result
solve  = undefined

solveForCoreVerbose :: Proposition -> Maybe VariantContext -> IO (VarCore, State)
solveForCoreVerbose  i (fromMaybe true -> conf) =
    T.runSMTWith T.z3{T.verbose=True} $
      do (_,iState) <- runSolverWith runStdoutLoggingT mempty $ unPre $ contextToSBool' conf
         (il, st) <- runSolverWith runStdoutLoggingT iState $ unPre $ toIL i
         C.query $ runSolverLog st $
           do core <- findVCore il
              logWith "Proposition: "  i
              logWith "Core: "         core
              logWith "Is Core Unit: " (isUnit core)
              return core

satVerbose :: Proposition -> Maybe VariantContext -> Settings -> IO Result
satVerbose = solveVerbose

sat :: Proposition -> Maybe VariantContext -> Settings -> IO Result
sat = solve

------------------------------ Async Helpers -----------------------------------
-- | season the solver, that is prepare it for async workloads
type Seasoning = T.Symbolic (IL, State)

-- | Producers actually produce two things: 1 they produce requests to other
-- produces when a new choice is found to solve a variant. 2 they produce
-- asynchronous results on the result channel
producer :: Seasoning -> T.SMTConfig -> State -> Int -> IO (a, State)
producer seasoning c _ tid = T.runSMTWith c $
  do (il, st) <- seasoning
     C.query $ runSolverLog st $
       do _ <- findVCore il
          forever $ do
            (_, requests) <- St.gets (fromJust . getWorkChans . workChans)
            logInThread tid "Waiting"
            continue <- liftIO $ U.readChan requests
            logInThread tid "Read a request, lets go"
            continue

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
type FromVC = U.OutChan Bool                -- the writer side, the bool value of a sat check
type ToVC   = U.InChan  (Dim, ShouldNegate) -- the reader side, a dim and flag

type FromMain = U.OutChan (Dim, ShouldNegate)
type ToMain   = U.InChan Bool

type WorkChanIn  = U.InChan  (Solver ())
type WorkChanOut = U.OutChan (Solver ())

type ToResultChan   = U.InChan  Result
type FromResultChan = U.OutChan Result

newtype VCChannels     = VCChannels     { getVcChans     :: Maybe (FromMain    , ToMain         ) }
newtype MainChannels   = MainChannels   { getMainChans   :: Maybe (FromVC      , ToVC           ) }
newtype WorkChannels   = WorkChannels   { getWorkChans   :: Maybe (WorkChanIn  , WorkChanOut    ) }
newtype ResultChannels = ResultChannels { getResultChans :: Maybe (ToResultChan, FromResultChan ) }

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


-- | That which contains that has that
class Has that where
  type Contains that
  type Contains that = State
  extract :: Contains that -> that
  wrap    :: that -> Contains that -> Contains that

  -- this `by` that
  by :: Contains that -> (that -> that) -> Contains that
  by this f = flip wrap this . f . extract $ this

-- avoiding lens and generic-deriving dependencies
instance Has Ints    where extract   = ints
                           wrap    i w = w{ints = i}

instance Has Doubles where extract   = doubles
                           wrap    d w = w{doubles = d}

instance Has Bools   where extract   = bools
                           wrap    b w = w{bools = b}

instance Has Result  where extract   = result
                           wrap    r w = w{result=r}

instance Has Dimensions where extract   = dimensions
                              wrap    d w = w{dimensions=d}

instance Has Context where extract     = config
                           wrap    d w = w{config=d}

instance Has (Maybe VariantContext) where extract     = vConfig
                                          wrap    d w = w{vConfig=d}


type SVariantContext = T.SBool
instance Semigroup SVariantContext where (<>) = (&&&)
instance Monoid    SVariantContext where mempty = true


-- | The internal state of the solver is just a record that accumulates results
-- and a configuration to track choice decisions. We make a trade off of memory
-- for speed and represent the configuration in several ways. We keep a setline
-- representation of the config to support setlike operations most notably
-- `member`, we keep a formula representation to send to the result module and
-- we keep the symbolic representation to send to the solver. If we were missing
-- any of these then we would need to translate one to the other which will cost
-- constant time _for every_ choice, hence we want to make that constant factor
-- as small as possible
data State = State
    { result      :: Result
    -- the formula representation of the config
    , vConfig     :: Maybe VariantContext -- the formula representation of the config
    -- a map or set representation of the config
    , config      :: Context -- a map or set representation of the config
    , ints        :: Ints
    , doubles     :: Doubles
    , bools       :: Bools
    , dimensions  :: Dimensions
    , vcChans     :: VCChannels
    , mainChans   :: MainChannels
    , workChans   :: WorkChannels
    , resultChans :: ResultChannels
    }

instance Show State where
  show State{..} = mconcat $
    zipWith (\x y -> x <> "  :  " <> y <> "\n")
    ["result","vConfig","config","ints","doubles","bools","dimensions"]
    [ show result -- ya hate to see it
    , show vConfig
    , show config
    , show ints
    , show doubles
    , show bools
    , show dimensions
    ]

instance Semigroup State where
  a <> b = State { result     = let !a' = result a
                                    !b' = result b
                                    in (a' <> b')
                 , config     = config  a <> config  b
                 , vConfig    = vConfig a <> vConfig b
                 , ints       = ints    a <> ints    b
                 , doubles    = doubles a <> doubles b
                 , bools      = bools   a <> bools   b
                 , dimensions = dimensions a <> dimensions b
                 , vcChans    = VCChannels $
                                getVcChans (vcChans a) <|>
                                getVcChans (vcChans b)
                 , mainChans  = MainChannels $
                                getMainChans (mainChans a) <|>
                                getMainChans (mainChans b)
                 , workChans  = WorkChannels $
                                getWorkChans (workChans a) <|>
                                getWorkChans (workChans b)
                 , resultChans = ResultChannels $
                                 getResultChans (resultChans a) <|>
                                 getResultChans (resultChans b)
                 }

instance Monoid State where
  mempty = State{ result      = mempty
                , config      = mempty
                , vConfig     = mempty
                , ints        = mempty
                , doubles     = mempty
                , bools       = mempty
                , dimensions  = mempty
                , vcChans     = VCChannels   Nothing
                , mainChans   = MainChannels Nothing
                , workChans   = WorkChannels   Nothing
                , resultChans = ResultChannels Nothing
                }

-- TODO remove the StateT dependency for ReaderT
-- | A solver is just a reader over a solver enabled monad. The reader
-- maintains information during the variational execution, such as
-- configuration, variable stores
newtype SolverT m a = SolverT { runSolverT :: St.StateT State m a }
  deriving ( Functor,Applicative,Monad,MonadIO -- base
           , MonadTrans, MonadError e, MonadLogger
           , St.MonadState State, T.MonadSymbolic, C.MonadQuery
           )

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
type Solver      = SolverT (LoggingT C.Query)
type SolverNoLog = SolverT (NoLoggingT C.Query)
newtype PreSolver m a = PreSolver { unPre :: SolverT m a }
  deriving (Functor, Applicative, Monad, St.MonadState State
           , MonadLogger, MonadIO, T.MonadSymbolic, C.MonadQuery)

runSolverWith :: (m (a, State) -> c) -> State -> SolverT m a -> c
runSolverWith f s = f . flip St.runStateT s . runSolverT

runSolverLog :: State -> Solver a -> C.Query (a, State)
runSolverLog = runSolverWith runStdoutLoggingT

-- runSolver = runSolverWith runNoLoggingT

class Show a => Constrainable m a b where cached :: a -> m b

-- TODO fix this duplication with derivingVia
instance (Monad m, T.MonadSymbolic m, C.MonadQuery m) =>
  Constrainable (SolverT m) Var IL where
  cached ref = do
    st <- St.get
    case find ref $ extract st of
      Just x -> return (Ref x)
      Nothing -> do
        newSym <- T.label (Text.unpack ref) <$> C.freshVar (Text.unpack ref)
        St.modify' (`by` add ref newSym)
        return (Ref newSym)

instance (MonadLogger m, Monad m, T.MonadSymbolic m, C.MonadQuery m) =>
  Constrainable (SolverT m) Dim T.SBool where
  cached d = do
    st <- St.get
    case find d $ extract st of
      Just x -> return x
      Nothing -> do
        let ref = Text.unpack $ getDim d
        newSym <- T.label ref <$> C.freshVar ref
        St.modify' (`by` add d newSym)
        return newSym

instance (Monad m, T.MonadSymbolic m, C.MonadQuery m) =>
  Constrainable (SolverT m) (ExRefType Var) IL' where
  cached (ExRefTypeI i) =
    do st <- St.get
       case find i $ extract st of
         Just x  -> return . Ref' . SI $ x
         Nothing -> do newSym <- T.label (Text.unpack i) <$> C.freshVar (Text.unpack i)
                       St.modify' (`by` add i newSym)
                       return (Ref' . SI $ newSym)

  cached (ExRefTypeD d) =
    do st <- St.get
       case find d $ extract st of
         Just x  -> return . Ref' $ SD x
         Nothing -> do newSym <- T.label (Text.unpack d) <$> C.freshVar (Text.unpack d)
                       St.modify' (`by` add d newSym)
                       return $! Ref' $ SD newSym

instance (Monad m, T.MonadSymbolic m) =>
  Constrainable (PreSolver m) Var IL where
  cached ref   = do st <- St.get
                    case find ref $ extract st of
                      Just x  -> return (Ref x)
                      Nothing -> do newSym <- T.sBool (Text.unpack ref)
                                    St.modify' (`by` add ref newSym)
                                    return (Ref newSym)


instance (Monad m, T.MonadSymbolic m) =>
  Constrainable (PreSolver m) (ExRefType Var) IL' where
  cached (ExRefTypeI i) =
    do st <- St.get
       case find i $ extract st of
         Just x  -> return . Ref' . SI $ x
         Nothing -> do newSym <- T.sInteger (Text.unpack i)
                       St.modify' (`by` add i newSym)
                       return (Ref' . SI $ newSym)

  cached (ExRefTypeD d) =
    do st <- St.get
       case find d $ extract st of
         Just x  -> return . Ref' $ SD x
         Nothing -> do newSym <- T.sDouble (Text.unpack d)
                       St.modify' (`by` add d newSym)
                       return $! Ref' $ SD newSym

instance (MonadLogger m, Monad m, T.MonadSymbolic m) =>
  Constrainable (PreSolver m) Dim T.SBool where
  cached d = do
    -- we write this one by hand
    ds <- St.gets dimensions
    case find d ds of
      Just x -> return x
      Nothing -> do
        let ref = Text.unpack $ getDim d
        newSym <- T.sBool ref
        St.modify' (`by` add d newSym)
        return newSym


----------------------------------- IL -----------------------------------------
type BRef = T.SBool

data NRef = SI T.SInteger
    | SD T.SDouble
    deriving Show

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
    | Ref BRef
    | BOp B_B IL
    | BBOp BB_B IL IL
    | IBOp NN_B IL' IL'
    | Chc Dim Proposition Proposition
    deriving Show

data IL' = Ref' NRef
    | IOp N_N IL'
    | IIOp NN_N IL' IL'
    | Chc' Dim NExpression NExpression
    deriving Show

-- TODO: factor out the redundant cases into a type class
-- | Convert a proposition into the intermediate language to generate a
-- Variational Core
toIL :: ( MonadLogger m
        , Constrainable m (ExRefType Var) IL'
        , Constrainable m Var IL) =>
        Prop' Var -> m IL
toIL (LitB True)   = return $! Ref T.sTrue
toIL (LitB False)  = return $! Ref T.sFalse
toIL (RefB ref)    = cached ref
toIL x@(OpB op (ChcB d l r)) = do logWith "Moving Op inside choice" x
                                  return $ Chc d (OpB op l) (OpB op r)
toIL (OpB op e)    = BOp op <$> toIL e
toIL (OpBB Impl l r) = do l' <- toIL l; r' <- toIL r; return $ BBOp Impl l' r'
toIL (OpBB Eqv l r)  = do l' <- toIL l; r' <- toIL r; return $ BBOp Eqv  l' r'
toIL (OpBB XOr l r)  = do l' <- toIL l; r' <- toIL r; return $ BBOp XOr  l' r'
toIL (OpBB And l r)  = do l' <- toIL l; r' <- toIL r; return $ BBOp And  l' r'
toIL (OpBB Or l r)   = do l' <- toIL l; r' <- toIL r; return $ BBOp Or   l' r'
toIL (OpIB op l r)   = do l' <- toIL' l; r' <- toIL' r; return $ IBOp op l' r'
toIL (ChcB d l r)    = return $ Chc d l r

toIL' :: ( Constrainable m (ExRefType Var) IL'
         , MonadLogger m) =>
         NExpr' Var -> m IL'
toIL' (LitI (I i))  = return . Ref' . SI $ T.literal i
toIL' (LitI (D d))  = return . Ref' . SD $ T.literal d
toIL' (RefI a)      = cached a
toIL' x@(OpI op (ChcI d l r)) = do logWith "Moving op inside chc" x
                                   return $ Chc' d (OpI op l) (OpI op r)
toIL' (OpI op e)    = IOp op <$> toIL' e
toIL' (OpII op l r) = do l' <- toIL' l; r' <- toIL' r; return $! IIOp op l' r'
toIL' (ChcI d l r)  = return $ Chc' d l r

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

-- | Accumulation: we purposefully are verbose to provide the optimizer better
-- opportunities. Accumulation seeks to combine as much as possible the plain
-- terms in the AST into symbolic references
accumulate :: IL -> IL
 -- computation rules
accumulate Unit                        = Unit
accumulate x@Ref{} = x
accumulate x@Chc{} = x
  -- bools
accumulate (BOp Not (Ref r))           = Ref $! bnot r
accumulate (BBOp And (Ref l) (Ref r))  = Ref $! l &&& r
accumulate (BBOp Or  (Ref l) (Ref r))  = Ref $! l ||| r
  -- numerics
accumulate (IBOp LT (Ref' l) (Ref' r))  = Ref $! l T..< r
accumulate (IBOp LTE (Ref' l) (Ref' r)) = Ref $! l T..<= r
accumulate (IBOp EQ (Ref' l) (Ref' r))  = Ref $! l T..== r
accumulate (IBOp NEQ (Ref' l) (Ref' r)) = Ref $! l T../= r
accumulate (IBOp GT (Ref' l) (Ref' r))  = Ref $! l T..>  r
accumulate (IBOp GTE (Ref' l) (Ref' r)) = Ref $! l T..>= r
  -- choices
accumulate x@(BBOp _ Chc {} Chc {})    = x
accumulate x@(BBOp _ (Ref _) Chc {})   = x
accumulate x@(BBOp _ Chc {} (Ref _))   = x
accumulate x@(IBOp _ Chc' {} Chc' {})  = x
accumulate x@(IBOp _ (Ref' _) Chc' {}) = x
accumulate x@(IBOp _ Chc' {} (Ref' _)) = x
 -- congruence rules
-- accumulate (BOp Not x@(IBOp _ _ _)) = let e' = accumulate x
--                                           res = BOp Not e' in
--                                         if isValue x
--                                         then accumulate res
--                                         else res

-- accumulate (BOp Not e)   = accumulate $ driveNotDown e
accumulate (BOp Not e) = let e'  = accumulate e
                             res = BOp Not e' in
                           if isValue e'
                           then accumulate res
                           else res
accumulate (BBOp op l r) = let l'  = accumulate l
                               r'  = accumulate r
                               res = BBOp op l' r' in
                             if isValue l' && isValue r'
                             then accumulate res
                             else res
accumulate (IBOp op l r) = let l' = accumulate' l
                               r' = accumulate' r
                               res = IBOp op l' r' in
                             if isValue' l' && isValue' r'
                             then accumulate res
                             else res

accumulate' :: IL' -> IL'
  -- computation rules
accumulate' x@(Ref' _)          = x
accumulate' (IOp Neg  (Ref' n)) = Ref' $! negate n
accumulate' (IOp Abs  (Ref' n)) = Ref' $! abs n
accumulate' (IOp Sign (Ref' n)) = Ref' $! signum n
accumulate' (IIOp Add (Ref' l) (Ref' r))  = Ref' $! l + r
accumulate' (IIOp Sub (Ref' l) (Ref' r))  = Ref' $! l - r
accumulate' (IIOp Mult (Ref' l) (Ref' r)) = Ref' $! l * r
accumulate' (IIOp Div (Ref' l) (Ref' r))  = Ref' $! l ./ r
accumulate' (IIOp Mod (Ref' l) (Ref' r))  = Ref' $! l .% r
  -- choices
accumulate' x@Chc' {}                     = x
accumulate' (IOp op (Chc' d l r))         = Chc' d (OpI op l) (OpI op r)
accumulate' x@(IIOp _ Chc' {} Chc' {})    = x
accumulate' x@(IIOp _ (Ref' _) Chc' {})   = x
accumulate' x@(IIOp _ Chc' {} (Ref' _))   = x
accumulate' (IIOp op c@Chc'{} r)   = IIOp op c $ accumulate' r
accumulate' (IIOp op l c@Chc'{})   = IIOp op (accumulate' l) c
  -- congruence rules
accumulate' (IOp o e)  = let e'  = accumulate' e
                             res = IOp o e' in
                         if isValue' e'
                         then accumulate' res
                         else res

accumulate' (IIOp o l r) = let l'  = accumulate' l
                               r'  = accumulate' r
                               res = IIOp o l' r' in
                               -- this check is required or else we may
                               -- infinitely recur on edge cases with choices
                               -- TODO encode the property in the type system to
                               -- avoid the check
                               if isValue' l' && isValue' r'
                               then accumulate' res
                               else res

-------------------------------- Evaluation -----------------------------------
toSolver :: (Monad m, I.SolverContext m) => T.SBool -> m VarCore
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
evaluate :: ( St.MonadState a m
            , MonadLogger m
            , Show a
            , I.SolverContext m) =>
            IL -> m VarCore
  -- computation rules
evaluate Unit     = do s <- St.get
                       logWith "Received Unit with State: " s
                       return $! intoCore Unit
evaluate (Ref b)  = toSolver b
evaluate x@Chc {} = return $! intoCore x
  -- bools
evaluate (BOp Not   (Ref r))         = toSolver $! bnot r
evaluate (BBOp And  (Ref l) (Ref r)) = toSolver $! l &&& r
evaluate (BBOp Or   (Ref l) (Ref r)) = toSolver $! l ||| r
  -- numerics
evaluate (IBOp LT  (Ref' l) (Ref' r)) = toSolver $! l .< r
evaluate (IBOp LTE (Ref' l) (Ref' r)) = toSolver $! l .<= r
evaluate (IBOp EQ  (Ref' l) (Ref' r)) = toSolver $! l .== r
evaluate (IBOp NEQ (Ref' l) (Ref' r)) = toSolver $! l ./= r
evaluate (IBOp GT  (Ref' l) (Ref' r)) = toSolver $! l .>  r
evaluate (IBOp GTE (Ref' l) (Ref' r)) = toSolver $! l .>= r
  -- choices
evaluate x@(BBOp _ Chc {}  Chc {})   = return $! intoCore x
evaluate x@(BBOp _ (Ref _) Chc {})   = return $! intoCore x
evaluate x@(BBOp _ Chc {} (Ref _))   = return $! intoCore x
evaluate x@(IBOp _ Chc' {} Chc' {})  = return $! intoCore x
evaluate x@(IBOp _ (Ref' _) Chc' {}) = return $! intoCore x
evaluate x@(IBOp _ Chc' {} (Ref' _)) = return $! intoCore x
  -- congruence cases
evaluate (BBOp And l Unit)      = evaluate l
evaluate (BBOp And Unit r)      = evaluate r
evaluate (BBOp And l x@(Ref _)) = do _ <- evaluate x; logWith "evalL " x; evaluate l
evaluate (BBOp And x@(Ref _) r) = do _ <- evaluate x; logWith "eval'ing R" x; evaluate r
evaluate x@(IBOp op l r)        =
  logWith "Found relation" x >>
  let l' = accumulate' l
      r' = accumulate' r
      res = IBOp op l' r' in
    if isValue' l' && isValue' r'
    then log "Trying to reduce relation" >> evaluate res
    else return $! intoCore res


  -- accumulation cases
evaluate x@(BOp Not _)  =  logWith "Unary Boolean Op in Eval" x >>
  do let x'  = accumulate x
     if isValue x' then evaluate x' else return $ intoCore x'
evaluate x@(BBOp And l r) = logWith "Eval And Recurring: " x >>
  do (VarCore l') <- evaluate l
     (VarCore r') <- evaluate r
     let res = BBOp And l' r'
     -- this is a hot loop, but we want to make the
     -- variational core as small as possible because it
     -- will pay off in the solver. Thus we perform a
     -- check here to determine if we can reduce the
     -- variational core even after a single pass
     if isValue l' || isValue r'
       then evaluate res
       else return $! intoCore res
evaluate x@(BBOp op l r)  = logWith "Eval General Recurring " x >>
  let l' = accumulate l
      r' = accumulate r
      res = BBOp op l' r' in
    if isValue l' && isValue r'
       then logWith "Reducing more" res >> evaluate res
    else do logWith "couldn't reduce" res
            logWith "Left " l'
            logWith "Right " r'
            return $! intoCore res

------------------------- Removing Choices -------------------------------------
-- TODO transform to a GADT
-- | We use a zipper to track the context when searching for choices, this
-- removes the need to perform tree rotations. We make this as strict as
-- possible because we know we will be consuming the entire structure so there
-- is not need to build thunks
data Ctx = InL Ctx BB_B IL
    | InR T.SBool BB_B Ctx
    | InLB Ctx NN_B IL'
    | InRB NRef NN_B Ctx
    | InL' Ctx NN_N IL'
    | InR' NRef NN_N Ctx
    | InU B_B Ctx
    | InU' N_N Ctx
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
findChoice (InBool l@Ref {} (InL parent op r@Ref {}))   = findChoice (InBool (accumulate  $ BBOp op l r) parent)
findChoice (InNum l@Ref' {} (InLB parent op r@Ref' {})) = findChoice (InBool (accumulate  $ IBOp op l r) parent)
findChoice (InNum l@Ref' {} (InL' parent op r@Ref' {})) = findChoice (InNum  (accumulate' $ IIOp op l r) parent)
  -- fold
findChoice (InBool r@Ref{} (InU o e))   = findChoice (InBool (accumulate $ BOp o r) e)
findChoice (InNum r@Ref'{}  (InU' o e)) = findChoice (InNum (accumulate' $ IOp o r) e)
findChoice (InBool r@Ref {} (InR acc op parent))  =
  findChoice (InBool (accumulate $ BBOp op (Ref acc) r) parent)
findChoice (InNum r@Ref' {} (InRB acc op parent)) =
  findChoice (InBool (accumulate $ IBOp op (Ref' acc) r) parent)
findChoice (InNum r@Ref' {} (InR' acc op parent)) =
  findChoice (InNum (accumulate' $ IIOp op (Ref' acc) r) parent)
  -- switch
findChoice (InBool (Ref l) (InL parent op r))   = findChoice (InBool r $ InR l op parent)
findChoice (InNum  (Ref' l) (InLB parent op r)) = findChoice (InNum  r $ InRB l op parent)
findChoice (InNum  (Ref' l) (InL' parent op r)) = findChoice (InNum  r $ InR' l op parent)
  -- recur
findChoice (InBool (BBOp op l r) ctx) = findChoice (InBool l $ InL ctx op r)
findChoice (InBool (IBOp op l r) ctx) = findChoice (InNum  l $ InLB ctx op r)
findChoice (InBool (BOp o e) ctx) = findChoice (InBool e $ InU o ctx)
findChoice (InNum (IOp o e) ctx) = findChoice (InNum e $ InU' o ctx)
findChoice (InNum (IIOp op l r) ctx) = findChoice (InNum  l $ InL' ctx op r)
  -- legal to discharge Units under a conjunction only
findChoice (InBool Unit (InL parent And r)) = findChoice (InBool r $ InR true And parent)
findChoice (InBool Unit (InR acc And parent)) = findChoice (InBool (Ref acc) parent)
  -- TODO Not sure if unit can ever exist with a context
findChoice x@(InBool Ref{} InU'{}) = error $ "Numeric unary operator applied to boolean: " ++ show x
findChoice x@(InNum Ref'{} InU{}) = error $ "Boolean unary operator applied to numeric: " ++ show x
findChoice (InBool Unit ctx) = error $ "Unit with a context" ++ show ctx
findChoice x@(InNum Ref'{} Top)    = error $ "Numerics can only exist in SMT within a relation: " ++ show x
findChoice x@(InBool Ref{} InLB{}) = error $ "An impossible case bool reference in inequality: "  ++ show x
findChoice x@(InBool Ref{} InRB{}) = error $ "An impossible case bool reference in inequality: "  ++ show x
findChoice x@(InBool Ref{} InL'{}) = error $ "An impossible case bool reference in arithmetic: " ++ show x
findChoice x@(InBool Ref{} InR'{}) = error $ "An impossible case bool reference in arithmetic: "  ++ show x
findChoice x@(InNum Ref'{} InL{}) = error $ "An impossible case: " ++ show x
findChoice x@(InNum Ref'{} InR{}) = error $ "An impossible case: " ++ show x


store :: (St.MonadState State io, MonadIO io) => Result -> io ()
{-# INLINE store #-}
{-# SPECIALIZE store :: Result -> Solver () #-}
store r = do (results,_) <- St.gets (fromJust . getResultChans . resultChans)
             liftIO $ U.writeChan results r

-- | TODO newtype this maybe stuff, this is an alternative instance
mergeVC :: Maybe VariantContext -> Maybe VariantContext -> Maybe VariantContext
mergeVC Nothing Nothing    = Nothing
mergeVC a@(Just _) Nothing = a
mergeVC Nothing b@(Just _) = b
mergeVC (Just l) (Just r)  = Just $ l &&& r

-- | A function that enforces each configuration is updated in sync
updateConfigs :: (St.MonadState State m) => Prop' Dim -> (Dim, Bool) -> m ()
{-# INLINE updateConfigs #-}
updateConfigs context (d,val) = do
  St.modify' (`by` (`mergeVC` (Just $ VariantContext context)))
  St.modify' (`by` add d val)

-- | Reset the state but maintain the cache's
resetTo :: (St.MonadState State m) => State -> m ()
resetTo s = do st <- St.get
               St.put s{ result=result st
                       , bools=bools st
                       , ints=ints st
                       , doubles=doubles st
                       , dimensions=dimensions st
                       }

-- | Given a dimensions and a way to continue with the left alternative, and a
-- way to continue with the right alternative. Spawn two new subprocesses that
-- process the alternatives plugging the choice hole with its respective
-- alternatives
-- alternative ::
--      ( St.MonadState State m
--      , MonadLogger m
--      , MonadIO m
--      , C.MonadQuery m
--      , Constrainable m Dim T.SBool)
--   => Dim -> m () -> m () -> m ()
alternative :: Dim -> Solver () -> Solver () -> Solver ()
alternative dim goLeft goRight =
  do conf <- St.gets config
     case find dim conf of
       Just True  -> log "Left Selected"  >> goLeft
       Just False -> log "Right Selected" >> goRight
       Nothing -> -- then this is a new dimension
         do s <- St.get -- cache the state
            let Just (fromVC, toVC) = getMainChans . mainChans $ s
                dontNegate          = False
                pleaseNegate        = True

            -- When we see a new dimension we check if both of its possible
            -- bindings is satisfiable, if so then we proceed to compute the
            -- variant, if not then we skip. This happens twice because
            -- dimensions and variant contexts can only be booleans.
            log "Reading dim for true"
            checkDimTrue <- liftIO $ U.writeChan toVC (dim, dontNegate) >> U.readChan fromVC
            when checkDimTrue $ do
              let continueLeft = C.inNewAssertionStack $
                    do log "Left Alternative"
                       updateConfigs (bRef dim) (dim,True)
                       goLeft
                  (requests,_) = fromJust . getWorkChans . workChans $ s
              log "Writing to go left"
              liftIO $ U.writeChan requests continueLeft

            -- reset for left side
            resetTo s

           -- right side, notice that we negate the symbolic
            log "Reading dim for false"
            checkDimFalse <- liftIO $ U.writeChan toVC (dim, pleaseNegate) >> U.readChan fromVC
            when checkDimFalse $ do
              let continueRight = C.inNewAssertionStack $
                    do log "Right Alternative"
                       updateConfigs (bnot $ bRef dim) (dim,False)
                       goRight
                  (requests,_) = fromJust . getWorkChans . workChans $ s

              log "Writing to continue right"
              liftIO $ U.writeChan requests continueRight


-- removeChoices :: ( MonadLogger m
--                  , St.MonadState State m
--                  , I.SolverContext m
--                  , C.MonadQuery m
--                  , MonadIO m
--                  , T.MonadSymbolic m
--                  , Constrainable m Var IL
--                  , Constrainable m Dim T.SBool
--                  , Constrainable m (ExRefType Var) IL'
--                  ) => VarCore -> m ()
  -- singleton instances
removeChoices :: VarCore -> Solver ()
{-# INLINE removeChoices #-}
removeChoices (VarCore Unit) = log "Core reduced to Unit" >>
                               St.get >>= getResult . vConfig >>= store
removeChoices (VarCore x@(Ref _)) = evaluate x >>= removeChoices
removeChoices (VarCore l) = choose (toLoc l)

-- choose :: ( St.MonadState State m
--           , I.SolverContext m
--           , MonadLogger m
--           , C.MonadQuery m
--           , T.MonadSymbolic m
--           , Constrainable m (ExRefType Var) IL'
--           , Constrainable m Var IL
--           , Constrainable m Dim T.SBool) =>
--           Loc -> m ()
choose :: Loc -> Solver ()
choose (InBool l@Ref{} Top) = evaluate l >>= removeChoices
choose loc =
  do
    let !loc' = findChoice loc
    case loc' of
      x@(InBool (Chc d cl cr) ctx) -> do
        logWith "Got choice in context: " x
        conf <- St.gets config
        let goLeft  = toIL cl >>= choose . findChoice . toLocWith ctx . accumulate
            goRight = toIL cr >>= choose . findChoice . toLocWith ctx . accumulate

        case find d conf of
          Just True  -> goLeft
          Just False -> goRight
          Nothing    -> alternative d goLeft goRight

      x@(InNum (Chc' d cl cr) ctx) -> do
        logWith "Got choice in context: " x
        conf <- St.gets config
        let goLeft  = toIL' cl >>= choose . findChoice . toLocWith' ctx . accumulate'
            goRight = toIL' cr >>= choose . findChoice . toLocWith' ctx . accumulate'

        case find d conf of
          Just True  -> goLeft
          Just False -> goRight
          Nothing    -> alternative d goLeft goRight

      (InBool Unit Top) -> removeChoices $ intoCore Unit
      x -> error $ "Choosing and missed cases with: " ++ show x


--------------------------- Variant Context Helpers ----------------------------
initVCWorker :: VariantContext -> VCChannels -> IO ()
initVCWorker vc (getVcChans -> Just (fromMain, toMain)) =
  T.runSMTWith T.z3{T.verbose=True} $ do
  -- season the solver
  (b,st) <- runSolverWith runStdoutLoggingT mempty $ unPre $ contextToSBool' vc
  T.constrain b
  -- enter query mode
  C.query $
  -- listen to the channel forever and check requests incrementally against the
  -- context
    void $
    runSolverWith runStdoutLoggingT st $
    forever $
     do (d, shouldNegate) <- C.io $ U.readChan fromMain
        C.inNewAssertionStack $
          do sD <- cached d
             T.constrain $ if shouldNegate then bnot sD else sD

             C.checkSat >>= C.io . \case
               C.Sat -> U.writeChan toMain True
               _     -> U.writeChan toMain False


 -- this can never happen and even if it does we want to stop
initVCWorker _ _ = error "passed no channels to initVCWorker!"

contextToSBool' :: VariantContext -> PreSolver (LoggingT (T.SymbolicT IO)) T.SBool
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

contextToSBool :: VariantContext -> Solver T.SBool
contextToSBool (getVarFormula -> x) = go x
  where -- go :: Show a => Prop' a -> m T.SBool
        go (LitB True)  = return T.sTrue
        go (LitB False) = return T.sFalse
        go (RefB d)     = do dims <- St.gets dimensions
                             case Map.lookup d dims of
                               Just b -> return b
                               Nothing -> do newSym <- T.label (Text.unpack $ getDim d) <$> C.freshVar (Text.unpack $ getDim d)
                                             St.modify' (`by` add d newSym)
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

instance Pretty IL where
  pretty Unit = "unit"
  pretty (Ref b)      = pretty b
  pretty (BOp Not r@(Ref _))   = "~" <> pretty r
  pretty (BOp o e)   = pretty o <> parens (pretty e)
  pretty (BBOp op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (IBOp op l r) = parens $ mconcat [pretty l, " ", pretty op, " ", pretty r]
  pretty (Chc d l r)  = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"

instance Pretty NRef where
  pretty (SI _) = "sInt"
  pretty (SD _) = "sDouble"

instance Pretty IL' where
  pretty (Ref' b)     = pretty b
  pretty (IOp o x@(Ref' _))  = pretty o <> pretty x
  pretty (IOp Abs e)  = between "|" (pretty e) "|"
  pretty (IOp o e)    = pretty o <> parens (pretty e)
  pretty (IIOp o l r) = parens $ mconcat [pretty l, " ", pretty o, " ", pretty r]
  pretty (Chc' d l r) = pretty d <> between "<" (pretty l <> "," <> pretty r) ">"
