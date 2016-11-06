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
       , GetLeaders (..)
       , GetLocalTxs (..)
       , GetLocalSscPayload (..)
       , GetToken (..)
       , GetOurShares (..)
       , GetThreshold (..)
       , GetParticipants (..)
       , MayBlockBeUseful (..)

       , CreateNewBlock (..)
       , ProcessBlock (..)
       , ProcessNewSlot (..)
       , ProcessSscMessage (..)
       , ProcessTx (..)
       , SetToken (..)

       , NewStatRecord (..)
       , GetStatRecords (..)
       ) where

import           Data.Acid            (EventResult, EventState, QueryEvent, UpdateEvent,
                                       makeAcidic)
import qualified Data.Acid            as Acid (Query)
import           Data.Default         (def)
import           Serokell.AcidState   (ExtendedState, closeExtendedState,
                                       openLocalExtendedState, openMemoryExtendedState,
                                       queryExtended, tidyExtendedState, updateExtended)
import           Serokell.Util        (VerificationRes)
import           Universum

import           Pos.Ssc.DynamicState (SscDynamicState)
import           Pos.State.Storage    (Storage)
import qualified Pos.State.Storage    as S
import           Pos.Types.Types      (Block, EpochIndex, HeaderHash, MainBlockHeader,
                                       SlotId, SlotLeaders)

----------------------------------------------------------------------------
-- Acid-state things
----------------------------------------------------------------------------

type DiskState = ExtendedState Storage

query
    :: (EventState event ~ Storage, QueryEvent event, MonadIO m)
    => DiskState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ Storage, UpdateEvent event, MonadIO m)
    => DiskState -> event -> m (EventResult event)
update = updateExtended

-- | Same as `openState`, but with explicitly specified initial state.
openStateCustom :: MonadIO m => Storage -> Bool -> FilePath -> m DiskState
openStateCustom customStorage deleteIfExists fp =
    openLocalExtendedState deleteIfExists fp customStorage

openState :: MonadIO m => Bool -> FilePath -> m DiskState
openState = openStateCustom def

openMemState :: MonadIO m => m DiskState
openMemState = openMemStateCustom def

openMemStateCustom :: MonadIO m => Storage -> m DiskState
openMemStateCustom = openMemoryExtendedState

closeState :: MonadIO m => DiskState -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => DiskState -> m ()
tidyState = tidyExtendedState

-- TODO: get rid of these (they are only needed temporarily)

getBlock :: HeaderHash SscDynamicState
         -> Acid.Query Storage (Maybe (Block SscDynamicState))
getBlock = S.getBlock

getLeaders :: EpochIndex -> Acid.Query Storage (Maybe SlotLeaders)
getLeaders = S.getLeaders

getHeadBlock :: Acid.Query Storage (Block SscDynamicState)
getHeadBlock = S.getHeadBlock

mayBlockBeUseful :: SlotId
                 -> MainBlockHeader SscDynamicState
                 -> Acid.Query Storage VerificationRes
mayBlockBeUseful = S.mayBlockBeUseful

makeAcidic ''Storage
    [ 'getBlock
    , 'S.getGlobalSscPayload
    , 'getLeaders
    , 'S.getLocalSscPayload
    , 'S.getLocalTxs
    , 'getHeadBlock
    , 'S.getToken
    , 'S.getOurShares
    , 'S.getThreshold
    , 'S.getParticipants
    , 'mayBlockBeUseful
    , 'S.createNewBlock
    , 'S.processBlock
    , 'S.processNewSlot
    , 'S.processSscMessage
    , 'S.processTx
    , 'S.setToken
    , 'S.newStatRecord
    , 'S.getStatRecords
    ]
