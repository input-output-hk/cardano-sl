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
       -- , addEntry
       -- , addLeaders
       -- , adoptBlock
       -- , createBlock
       -- , getLeader
       -- , getLeaders
       -- , setLeaders
       ) where

import           Control.Lens          (at, ix, makeClassy, preview, views, (%=), (.=),
                                        (<<.=))
import           Data.Acid             ()
import           Data.Default          (Default, def)
import           Data.SafeCopy         (base, deriveSafeCopySimple)
import qualified Data.Set              as Set (fromList, insert, toList, (\\))
import           Serokell.AcidState    ()
import           Universum

import           Pos.Crypto            (PublicKey)
import           Pos.State.Storage.Mpc (HasMpcStorage (mpcStorage), MpcStorage,
                                        getLeaders)
import           Pos.State.Storage.Tx  (HasTxStorage (txStorage), TxStorage, addTx)
import           Pos.Types             (Block, HeaderHash, SlotId, Utxo, unflattenSlotId)

type Query  a = forall m . MonadReader Storage m => m a
type Update a = forall m . MonadState Storage m => m a

data Storage = Storage
    { -- | State of MPC
      __mpcStorage :: !MpcStorage
    , -- | State of transaction-handling worker
      __txStorage  :: !TxStorage
    , -- | Id of last seen slot.
      _slotId      :: !SlotId
    , -- | The best valid blockchain known to the node. We should take
      -- into account that we are dealing with tree, not list. This list
      -- is the best chain. We use builtin lists for simplicity,
      -- because we don't care about performance for now.
      _blocks      :: [Block]
    , -- | Extra blocks from alternative chains. It serves as cache basically.
      _extraBlocks :: !(HashMap HeaderHash Block)
    }

makeClassy ''Storage
deriveSafeCopySimple 0 'base ''Storage

instance HasMpcStorage Storage where
    mpcStorage = _mpcStorage
instance HasTxStorage Storage where
    txStorage = _txStorage

instance Default Storage where
    def =
        Storage
        { __mpcStorage = def
        , __txStorage = def
        , _slotId = unflattenSlotId 0
        , _blocks = mempty
        , _extraBlocks = mempty
        }

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
