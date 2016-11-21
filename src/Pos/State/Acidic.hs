{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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

       , GetBlock (..)
       , GetGlobalSscPayload (..)
       , GetHeadBlock (..)
       , GetBestChain (..)
       , GetLeaders (..)
       , GetLocalTxs (..)
       , IsTxVerified (..)
       , GetLocalSscPayload (..)
       , GetOurShares (..)
       , GetThreshold (..)
       , GetParticipants (..)
       , MayBlockBeUseful (..)

       , CreateNewBlock (..)
       , ProcessBlock (..)
       , ProcessNewSlot (..)
       , ProcessSscMessage (..)
       , ProcessTx (..)

       ) where

import           Data.Acid             (EventResult, EventState, QueryEvent, UpdateEvent,
                                        makeAcidicWithHacks)
import           Data.Default          (Default, def)
import           Data.SafeCopy         (SafeCopy)
import           Serokell.AcidState    (ExtendedState, closeExtendedState,
                                        openLocalExtendedState, openMemoryExtendedState,
                                        queryExtended, tidyExtendedState, updateExtended)
import           Universum

import           Pos.Ssc.Class.Storage (SscStorageClass (..))
import           Pos.Ssc.Class.Types   (Ssc (SscStorage))
import qualified Pos.State.Storage     as S

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

-- | Open disk state. Accepts \"deleteIfExists\" flag and filepath.
openState
    :: (SscStorageClass ssc, Default (SscStorage ssc), SafeCopy ssc, MonadIO m)
    => Bool -> FilePath -> m (DiskState ssc)
openState = openStateCustom def

-- | Same as 'openState', but with explicitly specified initial
-- state.
openStateCustom :: (SscStorageClass ssc, SafeCopy ssc, MonadIO m)
                => Storage ssc
                -> Bool
                -> FilePath
                -> m (DiskState ssc)
openStateCustom customStorage deleteIfExists fp =
    openLocalExtendedState deleteIfExists fp customStorage


-- | Open in-ram state.
openMemState
    :: (SscStorageClass ssc, Default (SscStorage ssc), SafeCopy ssc, MonadIO m)
    => m (DiskState ssc)
openMemState = openMemStateCustom def

-- | Same as 'openMemState', but with explicitly specified initial state
openMemStateCustom
    :: (SscStorageClass ssc, SafeCopy ssc, MonadIO m)
    => Storage ssc -> m (DiskState ssc)
openMemStateCustom = openMemoryExtendedState

-- | Closes the state.
closeState :: (SscStorageClass ssc, MonadIO m) => DiskState ssc -> m ()
closeState = closeExtendedState

-- | Removes history from the state, making it smaller in size.
tidyState :: (SscStorageClass ssc, MonadIO m) => DiskState ssc -> m ()
tidyState = tidyExtendedState

makeAcidicWithHacks ''S.Storage ["ssc"]
    [ 'S.getBlock
    , 'S.getGlobalSscPayload
    , 'S.getLeaders
    , 'S.getLocalSscPayload
    , 'S.getLocalTxs
    , 'S.isTxVerified
    , 'S.getHeadBlock
    , 'S.getBestChain
    , 'S.getOurShares
    , 'S.getThreshold
    , 'S.getParticipants
    , 'S.mayBlockBeUseful
    , 'S.createNewBlock
    , 'S.processBlock
    , 'S.processNewSlot
    , 'S.processSscMessage
    , 'S.processTx
    ]
