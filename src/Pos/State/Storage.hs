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

import           Control.Lens       (at, ix, makeLenses, preview, views, (%=), (.=),
                                     (<<.=))
import           Data.Acid          ()
import           Data.Default       (Default, def)
import           Data.SafeCopy      (base, deriveSafeCopySimple)
import qualified Data.Set           as Set (fromList, insert, toList, (\\))
import           Safe               (atMay)
import           Serokell.AcidState ()
import           Universum

import           Pos.Crypto         (PublicKey)
import           Pos.Slotting       (unflattenSlotId)
import           Pos.Types          (Block, EpochIndex, HeaderHash, SlotId, Tx, Utxo)

type Query  a = forall m . MonadReader Storage m => m a
type Update a = forall m . MonadState Storage m => m a

data Storage = Storage
    { -- | Id of last seen slot.
      _slotId      :: !SlotId
    , -- | The best valid blockchain known to the node. We should take
      -- into account that we are dealing with tree, not list. This list
      -- is the best chain. We use builtin lists for simplicity,
      -- because we don't care about performance for now.
      _blocks      :: [Block]
    , -- | Extra blocks from alternative chains. It serves as cache basically.
      _extraBlocks :: !(HashMap HeaderHash Block)
    , -- | Set of unspent transaction outputs. It is need to check new
      -- transactions and run follow-the-satoshi, for example.
      _utxo        :: !Utxo
    , -- | Local set of transactions. These are valid (with respect to
      -- utxo) transactions which are known to the node and are not
      -- included in the blockchain store by the node.
      _txs         :: !(HashSet Tx)
    }

makeLenses ''Storage
deriveSafeCopySimple 0 'base ''Storage

instance Default Storage where
    def =
        Storage
        { _slotId = unflattenSlotId 0
        , _blocks = mempty
        , _extraBlocks = mempty
        , _utxo = mempty
        , _txs = mempty
        }

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: EpochIndex -> Query [PublicKey]
getLeaders _ = pure []

-- | Add transaction to storage if it is fully valid. Returns True iff
-- transaction has been added.
addTx :: Tx -> Update Bool
addTx _ = pure False

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
