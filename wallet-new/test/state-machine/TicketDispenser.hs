{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TicketDispenser
-- Copyright   :  (C) 2017, ATS Advanced Telematic Systems GmbH
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Stevan Andjelkovic <stevan.andjelkovic@here.com>
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- This module contains a specification of a simple ticket dispenser.
--
-----------------------------------------------------------------------------

module TicketDispenser
  ( prop_ticketDispenser
  , prop_ticketDispenserParallel
  , prop_ticketDispenserParallelOK
  , prop_ticketDispenserParallelBad
  , withDbLock
  , setupLock
  , cleanupLock
  )
  where

import           Control.Exception (IOException, catch)
import           Data.TreeDiff (ToExpr)
import           GHC.Generics (Generic, Generic1)
import           Prelude hiding (readFile)
import           System.Directory (createDirectoryIfMissing,
                     getTemporaryDirectory, removeFile)
import           System.FileLock (SharedExclusive (..), lockFile, unlockFile)
import           System.FilePath ((</>))
import           System.IO (hClose, openTempFile)
import           System.IO.Strict (readFile)
import           Test.QuickCheck (Gen, Property, frequency, (===))
import           Test.QuickCheck.Monadic (monadicIO)

import           Test.StateMachine
import qualified Test.StateMachine.Types.Rank2 as Rank2

------------------------------------------------------------------------

-- The actions of the ticket dispenser are:

data Action (r :: * -> *)
  = TakeTicket
  | Reset
  deriving (Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

data Response (r :: * -> *)
  = GotTicket Int
  | ResetOk
  deriving (Show, Generic1, Rank2.Foldable)

-- Which correspond to taking a ticket and getting the next number, and
-- resetting the number counter of the dispenser.

------------------------------------------------------------------------

-- The dispenser has to be reset before use, hence the maybe integer.

newtype Model (r :: * -> *) = Model (Maybe Int)
  deriving (Eq, Show, Generic)

deriving instance ToExpr (Model Concrete)

initModel :: Model r
initModel = Model Nothing

preconditions :: Model Symbolic -> Action Symbolic -> Logic
preconditions (Model Nothing)  TakeTicket = Bot
preconditions (Model (Just _)) TakeTicket = Top
preconditions _                Reset      = Top

transitions :: Model r -> Action r -> Response r -> Model r
transitions (Model m) cmd _ = case cmd of
  TakeTicket -> Model (succ <$> m)
  Reset      -> Model (Just 0)

postconditions :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
postconditions (Model m) TakeTicket (GotTicket n) = Just n .== (succ <$> m)
postconditions _         Reset      ResetOk       = Top
postconditions _         _          _             = error "postconditions"

------------------------------------------------------------------------

-- With stateful generation we ensure that the dispenser is reset before
-- use.

generator :: Model Symbolic -> Gen (Action Symbolic)
generator _ = frequency
  [ (1, pure Reset)
  , (4, pure TakeTicket)
  ]

shrinker :: Action Symbolic -> [Action Symbolic]
shrinker _ = []

------------------------------------------------------------------------

-- We will implement the dispenser using a simple database file which
-- stores the next number. A file lock is used to allow concurrent use.

semantics
  :: SharedExclusive                       -- ^ Indicates if the file
                                           -- lock should be shared
                                           -- between threads or if it
                                           -- should be exclusive.
                                           -- Sharing it could cause
                                           -- race conditions.

  -> (FilePath, FilePath)                  -- ^ File paths to the
                                           -- database storing the
                                           -- ticket counter and the
                                           -- file lock used for
                                           -- synchronisation.

  -> Action Concrete

  -> IO (Response Concrete)

semantics se (tdb, tlock) cmd = case cmd of
  TakeTicket -> do
    lock <- lockFile tlock se
    i <- read <$> readFile tdb
      `catch` (\(_ :: IOException) -> return "-1")
    writeFile tdb (show (i + 1))
      `catch` (\(_ :: IOException) -> return ())
    unlockFile lock
    return (GotTicket (i + 1))
  Reset      -> do
    lock <- lockFile tlock se
    writeFile tdb (show (0 :: Integer))
      `catch` (\(_ :: IOException) -> return ())
    unlockFile lock
    return ResetOk

mock :: Model Symbolic -> Action Symbolic -> GenSym (Response Symbolic)
-- mock (Model Nothing)  TakeTicket = error "mock: TakeTicket"
-- mock (Model (Just n)) TakeTicket = GotTicket <$> pure n
mock _                _      = pure ResetOk

------------------------------------------------------------------------

type DbLock = (FilePath, FilePath)

setupLock :: IO DbLock
setupLock = do
  tmp <- getTemporaryDirectory
  let tmpTD = tmp </> "ticket-dispenser"
  createDirectoryIfMissing True tmpTD
  (tdb,   dbh)   <- openTempFile tmpTD "ticket-dispenser.db"
  hClose dbh
  (tlock, lockh) <- openTempFile tmpTD "ticket-dispenser.lock"
  hClose lockh
  return (tdb, tlock)

cleanupLock :: DbLock -> IO ()
cleanupLock (tdb, tlock) = do
  removeFile tdb
  removeFile tlock

withDbLock :: (DbLock -> IO ()) -> IO ()
withDbLock run = do
  lock <- setupLock
  run lock
  cleanupLock lock

sm :: SharedExclusive -> DbLock -> StateMachine Model Action IO Response
sm se files = StateMachine
  initModel transitions preconditions postconditions
  Nothing generator Nothing shrinker (semantics se files) mock

-- Sequentially the model is consistent (even though the lock is
-- shared).

prop_ticketDispenser :: DbLock -> Property
prop_ticketDispenser files = forAllCommands sm' Nothing $ \cmds -> monadicIO $ do
  (hist, _, res) <- runCommands sm' cmds
  prettyCommands sm' hist $
    checkCommandNames cmds (res === Ok)
  where
    sm' = sm Shared files

prop_ticketDispenserParallel :: SharedExclusive -> DbLock -> Property
prop_ticketDispenserParallel se files =
  forAllParallelCommands sm' $ \cmds -> monadicIO $
    prettyParallelCommands cmds =<< runParallelCommandsNTimes 100 sm' cmds
  where
    sm' = sm se files

-- So long as the file locks are exclusive, i.e. not shared, the
-- parallel property passes.
prop_ticketDispenserParallelOK :: DbLock -> Property
prop_ticketDispenserParallelOK = prop_ticketDispenserParallel Exclusive

prop_ticketDispenserParallelBad :: DbLock -> Property
prop_ticketDispenserParallelBad = prop_ticketDispenserParallel Shared
