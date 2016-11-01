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
       , openMemState
       , tidyState

       , query
       , update

       , GetBlock (..)
       , GetHeadBlock (..)
       , GetLeaders (..)
       , GetLocalTxs (..)
       , GetLocalMpcData (..)
       , GetGlobalMpcData (..)
       , GetSecret (..)
       , GetOurCommitment (..)
       , GetOurOpening (..)
       , GetOurShares (..)
       , GetThreshold (..)
       , GetParticipants (..)
       , MayBlockBeUseful (..)

       , CreateNewBlock (..)
       , ProcessBlock (..)
       , ProcessNewSlot (..)
       , ProcessCommitment (..)
       , ProcessOpening (..)
       , ProcessShares (..)
       , ProcessTx (..)
       , ProcessVssCertificate (..)
       , SetSecret (..)

       , AddStatRecord (..)
       , GetStatRecords (..)
       ) where

import           Data.Acid                  (EventResult, EventState, QueryEvent,
                                             UpdateEvent, makeAcidic)
import qualified Data.Acid                  as Acid (Query)
import           Data.Default               (def)
import           Serokell.AcidState         (ExtendedState, closeExtendedState,
                                             openLocalExtendedState,
                                             openMemoryExtendedState, queryExtended,
                                             tidyExtendedState, updateExtended)
import           Serokell.Util              (VerificationRes)
import           Universum

import           Pos.Ssc.DynamicState.Types (SscDynamicState)
import           Pos.State.Storage          (Storage)
import qualified Pos.State.Storage          as S
import           Pos.Types.Types            (Block, EpochIndex, HeaderHash,
                                             MainBlockHeader, SlotId, SlotLeaders)

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

openState :: MonadIO m => Bool -> FilePath -> m DiskState
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp def

openMemState :: MonadIO m => m DiskState
openMemState = openMemoryExtendedState def

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
    , 'getLeaders
    , 'S.getLocalTxs
    , 'S.getLocalMpcData
    , 'S.getGlobalMpcData
    , 'getHeadBlock
    , 'S.getSecret
    , 'S.getOurCommitment
    , 'S.getOurOpening
    , 'S.getOurShares
    , 'S.getThreshold
    , 'S.getParticipants
    , 'mayBlockBeUseful
    , 'S.createNewBlock
    , 'S.processBlock
    , 'S.processNewSlot
    , 'S.processCommitment
    , 'S.processOpening
    , 'S.processShares
    , 'S.processTx
    , 'S.processVssCertificate
    , 'S.setSecret
    , 'S.addStatRecord
    , 'S.getStatRecords
    ]
