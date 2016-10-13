{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Storage with node local state which should be persistent.

module Pos.State.Storage
       (
         Storage

       , Query
       , getLeaders

       , Update
       , addTx
       , processNewSlot
       -- , addEntry
       -- , addLeaders
       -- , adoptBlock
       -- , createBlock
       -- , getLeader
       -- , getLeaders
       -- , setLeaders
       ) where

import           Control.Lens            (makeClassy, use, (.=))
import           Data.Acid               ()
import           Data.Default            (Default, def)
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Serokell.AcidState      ()
import           Universum

import           Pos.State.Storage.Block (BlockStorage, HasBlockStorage (blockStorage),
                                          getLeaders)
import           Pos.State.Storage.Mpc   (HasMpcStorage (mpcStorage), MpcStorage)
import           Pos.State.Storage.Tx    (HasTxStorage (txStorage), TxStorage, addTx)
import           Pos.Types               (SlotId, unflattenSlotId)

type Query  a = forall m . MonadReader Storage m => m a
type Update a = forall m . MonadState Storage m => m a

data Storage = Storage
    { -- | State of MPC.
      __mpcStorage   :: !MpcStorage
    , -- | Transactions part of /static-state/.
      __txStorage    :: !TxStorage
    , -- | Blockchain part of /static-state/.
      __blockStorage :: !BlockStorage
    , -- | Id of last seen slot.
      _slotId        :: !SlotId
    }

makeClassy ''Storage
deriveSafeCopySimple 0 'base ''Storage

instance HasMpcStorage Storage where
    mpcStorage = _mpcStorage
instance HasTxStorage Storage where
    txStorage = _txStorage
instance HasBlockStorage Storage where
    blockStorage = _blockStorage

instance Default Storage where
    def =
        Storage
        { __mpcStorage = def
        , __txStorage = def
        , __blockStorage = def
        , _slotId = unflattenSlotId 0
        }

-- | Do all necessary changes when new slot starts.
processNewSlot :: SlotId -> Update ()
processNewSlot sId = do
    knownSlot <- use slotId
    when (sId > knownSlot) $ processNewSlotDo sId

-- TODO
processNewSlotDo :: SlotId -> Update ()
processNewSlotDo sId = slotId .= sId

-- createBlock :: Update Blockkk
-- createBlock = do
--     es <- pendingEntries <<.= mempty
--     return (Set.toList es)

-- addLeaders :: Int -> [NodeId] -> Update ()
-- addLeaders epoch leaders =
--     pendingEntries %= Set.insert (ELeaders (epoch + 1) leaders)

-- getLeader :: Int -> Int -> Query (Maybe NodeId)
-- getLeader epoch slot = preview (epochLeaders . ix epoch . ix slot)

-- addEntry :: Entry -> Update ()
-- addEntry e = pendingEntries %= Set.insert e

-- adoptBlock :: Blockkk -> Update ()
-- adoptBlock es = do
--     pendingEntries %= (Set.\\ Set.fromList es)
--     blocks %= (es :)

-- setLeaders :: Int -> [NodeId] -> Update ()
-- setLeaders epoch leaders = epochLeaders . at epoch .= Just leaders
