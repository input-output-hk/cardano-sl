{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Acid-state wrapped operations.

module Pos.State.Acidic
       ( DiskState
       , closeState
       , openState
       , openStateCustom
       , openMemState
       , openMemStateCustom
       , tidyState

       , query
       , update
       , updateWithLog

       , GetBlock (..)
       , GetBlockByDepth (..)
       , GetGlobalSscState (..)
       , GetHeadBlock (..)
       , GetBestChain (..)
       , GetLeaders (..)
       , GetLocalTxs (..)
       , IsTxVerified (..)
       , GetOurShares (..)
       , GetParticipants (..)
       , MayBlockBeUseful (..)

       , CreateNewBlock (..)
       , ProcessBlock (..)
       , ProcessTx (..)

         -- * Updates with logging
       , ProcessNewSlotL (..)
       ) where

import           Universum

import           Control.Monad.State   (MonadState)
import           Data.Acid             (EventResult, EventState, QueryEvent, Update,
                                        UpdateEvent, makeAcidicWithHacks)
import           Data.Default          (Default, def)
import           Data.SafeCopy         (SafeCopy)
import           Serokell.AcidState    (ExtendedState, closeExtendedState,
                                        openLocalExtendedState, openMemoryExtendedState,
                                        queryExtended, tidyExtendedState, updateExtended)
import           System.Wlog           (CanLog, HasLoggerName (..), LogEvent, LoggerName,
                                        LoggerNameBox (..), PureLogger, runPureLog,
                                        usingLoggerName)

import           Pos.Ssc.Class.Helpers (SscHelpersClass)
import           Pos.Ssc.Class.Storage (SscStorageClass (..))
import           Pos.Ssc.Class.Types   (Ssc (SscStorage))
import qualified Pos.State.Storage     as S
import           Pos.Types             as PT

----------------------------------------------------------------------------
-- Acid-state things
----------------------------------------------------------------------------

type Storage ssc = S.Storage ssc
type DiskState ssc = ExtendedState (Storage ssc)

-- | 'queryExtended' specification for 'Storage'.
query
    :: (SscStorageClass ssc, EventState event ~ Storage ssc,
        QueryEvent event, MonadIO m)
    => DiskState ssc -> event -> m (EventResult event)
query = queryExtended

-- | 'updateExtended' specification for 'Storage'.
update
    :: (SscStorageClass ssc, EventState event ~ Storage ssc,
        UpdateEvent event, MonadIO m)
    => DiskState ssc -> event -> m (EventResult event)
update = updateExtended

-- | 'updateExtended' specification for 'Storage' with logged results.
updateWithLog
    :: ( SscStorageClass ssc
       , EventState event ~ Storage ssc
       , EventResult event ~ (a, [LogEvent])
       , UpdateEvent event
       , MonadIO m
       , HasLoggerName m)
    => DiskState ssc
    -> (LoggerName -> event)
    -> m (a, [LogEvent])
updateWithLog disk loggedEvent = do
    event <- modifyLoggerName (<> "acid") $ loggedEvent <$> getLoggerName
    updateExtended disk event

-- | Open disk state. Accepts \"deleteIfExists\" flag and filepath.
openState
    :: (SscHelpersClass ssc, SscStorageClass ssc, Default (SscStorage ssc), SafeCopy ssc, MonadIO m)
    => Bool -> FilePath -> m (DiskState ssc)
openState = openStateCustom def

-- | Same as 'openState', but with explicitly specified initial
-- state.
openStateCustom :: (SscHelpersClass ssc, SscStorageClass ssc, SafeCopy ssc, MonadIO m)
                => Storage ssc
                -> Bool
                -> FilePath
                -> m (DiskState ssc)
openStateCustom customStorage deleteIfExists fp =
    openLocalExtendedState deleteIfExists fp customStorage

-- | Open in-ram state.
openMemState
    :: (SscHelpersClass ssc, SscStorageClass ssc, Default (SscStorage ssc), SafeCopy ssc, MonadIO m)
    => m (DiskState ssc)
openMemState = openMemStateCustom def

-- | Same as 'openMemState', but with explicitly specified initial state
openMemStateCustom
    :: (SscHelpersClass ssc, SscStorageClass ssc, SafeCopy ssc, MonadIO m)
    => Storage ssc -> m (DiskState ssc)
openMemStateCustom = openMemoryExtendedState

-- | Closes the state.
closeState :: (SscStorageClass ssc, MonadIO m) => DiskState ssc -> m ()
closeState = closeExtendedState

-- | Removes history from the state, making it smaller in size.
tidyState :: (SscStorageClass ssc, MonadIO m) => DiskState ssc -> m ()
tidyState = tidyExtendedState

-- | Class that allows converting acid-state updates with logging into simple updates.
class ConvertUpdateWithLog l u  | u -> l where
    convertUpdateWithLog :: l -> u

-- | Wrapper for @acid-state@ updates which supports logging and logger names.
newtype LogUpdate s a = LogUpdate
    { runLogUpdate :: PureLogger (LoggerNameBox (Update s)) a
    } deriving (Functor, Applicative, Monad, CanLog, HasLoggerName, MonadState s)

unwrapLogUpdate :: LogUpdate s a -> LoggerName -> Update s (a, [LogEvent])
unwrapLogUpdate lu name = usingLoggerName name $ runPureLog $ runLogUpdate lu

instance ConvertUpdateWithLog
    (LogUpdate storage a)
    (LoggerName -> Update storage (a, [LogEvent]))
  where
    convertUpdateWithLog = unwrapLogUpdate

instance ConvertUpdateWithLog
    (arg1 -> LogUpdate storage a)
    (arg1 -> LoggerName -> Update storage (a, [LogEvent]))
  where
    convertUpdateWithLog logF = unwrapLogUpdate . logF

instance ConvertUpdateWithLog
    (arg1 -> arg2 -> LogUpdate storage a)
    (arg1 -> arg2 -> LoggerName -> Update storage (a, [LogEvent]))
  where
    convertUpdateWithLog logF arg1 = unwrapLogUpdate . logF arg1

----------------------------------------------------------------------------
-- Converted updates with logging
----------------------------------------------------------------------------

-- | Convenient alias to use in updates with logging.
type UpdateWithLog ssc a = Update (Storage ssc) (a, [LogEvent])

processNewSlotL
    :: (SscStorageClass ssc)
    => PT.SlotId
    -> LoggerName
    -> UpdateWithLog ssc (Maybe (PT.GenesisBlock ssc))
processNewSlotL = convertUpdateWithLog S.processNewSlot

----------------------------------------------------------------------------
-- Making everything acidic
----------------------------------------------------------------------------

makeAcidicWithHacks ''S.Storage ["ssc"]
    [ 'S.getBlock
    , 'S.getBlockByDepth
    , 'S.getGlobalSscState
    , 'S.getLeaders
    , 'S.getLocalTxs
    , 'S.isTxVerified
    , 'S.getHeadBlock
    , 'S.getBestChain
    , 'S.getOurShares
    , 'S.getParticipants
    , 'S.mayBlockBeUseful
    , 'S.createNewBlock
    , 'S.processBlock
    , 'S.processTx

    , 'processNewSlotL
    ]
