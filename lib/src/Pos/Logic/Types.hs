{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pos.Logic.Types
    ( Logic (..)
    , hoistLogic
    , dummyLogic
    , KeyVal (..)
    , hoistKeyVal
    , dummyKeyVal
    ) where

import           Universum

import           Data.Conduit (ConduitT, transPipe)
import           Data.Default (def)
import           Data.Tagged (Tagged)

import           Pos.Chain.Block (Block, BlockHeader, HeaderHash)
import           Pos.Chain.Security (SecurityParams (..))
import           Pos.Chain.Ssc (MCCommitment, MCOpening, MCShares,
                     MCVssCertificate)
import           Pos.Communication (NodeId)
import           Pos.Core (StakeholderId)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Core.Delegation (ProxySKHeavy)
import           Pos.Core.Txp (TxId, TxMsgContents)
import           Pos.Core.Update (BlockVersionData, UpId, UpdateProposal,
                     UpdateVote, VoteId)
import           Pos.DB.Block (GetHashesRangeError, GetHeadersFromManyToError)
import           Pos.DB.Class (SerializedBlock)

-- | The interface to a logic layer, i.e. some component which encapsulates
-- blockchain / crypto logic.
data Logic m = Logic
    { -- | The stakeholder id of our node.
      ourStakeholderId   :: StakeholderId
      -- | Get serialized block, perhaps from a database.
    , getSerializedBlock :: HeaderHash -> m (Maybe SerializedBlock)
    , streamBlocks       :: HeaderHash -> ConduitT () SerializedBlock m ()
      -- | Get a block header.
    , getBlockHeader     :: HeaderHash -> m (Maybe BlockHeader)
      -- TODO CSL-2089 use conduits in this and the following methods
      -- | Retrieve block header hashes from specified interval.
    , getHashesRange     :: Maybe Word -- Optional limit on how many to bring in.
                         -> HeaderHash
                         -> HeaderHash
                         -> m (Either GetHashesRangeError (OldestFirst NE HeaderHash))
      -- | Interface for 'getHeadersFromManyTo'. Retrieves blocks from
      -- the checkpoints to some particular point (or tip, if
      -- 'Nothing').
    , getBlockHeaders    :: Maybe Word -- Optional limit on how many to bring in.
                         -> NonEmpty HeaderHash
                         -> Maybe HeaderHash
                         -> m (Either GetHeadersFromManyToError (NewestFirst NE BlockHeader))
      -- | Compute LCA with the main chain: the first component are those hashes
      -- which are in the main chain, second is those which are not.
      -- Input is assumed to be a valid chain: if some element is not in the
      -- chain, then none of the later elements are.
    , getLcaMainChain    :: OldestFirst [] HeaderHash
                         -> m (NewestFirst [] HeaderHash, OldestFirst [] HeaderHash)
      -- | Get the current tip of chain.
    , getTip             :: m Block
      -- | Cheaper version of 'headerHash <$> getTip'.
    , getTipHeader       :: m BlockHeader
      -- | Get state of last adopted BlockVersion. Related to update system.
    , getAdoptedBVData   :: m BlockVersionData

      -- | Give a block header to the logic layer.
      -- NodeId is needed for first iteration, but will be removed later.
    , postBlockHeader    :: BlockHeader -> NodeId -> m ()

      -- | Tx, update, ssc...
      -- Common pattern is:
      --   - What to do with it when we receive it (key and data).
      --   - How to get it when it's requested (key).
      --   - Whether to request it when it's announced (key).
      --   - How to derive the key from the data.
      --     Needed in case we get data, so that we can use the key to
      --     announce it.
      -- For the first iteration we'll keep changes to this inv/req/data
      -- system minimal, so the logic layer must define how to do all of
      -- these things for every relayed piece of data.
      -- See comment on the 'KeyVal' type.
    , postTx             :: KeyVal (Tagged TxMsgContents TxId) TxMsgContents m
    , postUpdate         :: KeyVal (Tagged (UpdateProposal, [UpdateVote]) UpId) (UpdateProposal, [UpdateVote]) m
    , postVote           :: KeyVal (Tagged UpdateVote VoteId) UpdateVote m
    , postSscCommitment  :: KeyVal (Tagged MCCommitment StakeholderId) MCCommitment m
    , postSscOpening     :: KeyVal (Tagged MCOpening StakeholderId) MCOpening m
    , postSscShares      :: KeyVal (Tagged MCShares StakeholderId) MCShares m
    , postSscVssCert     :: KeyVal (Tagged MCVssCertificate StakeholderId) MCVssCertificate m

      -- | Give a heavy delegation certificate. Returns False if something
      -- went wrong.
    , postPskHeavy       :: ProxySKHeavy -> m Bool

      -- Recovery mode related stuff.
      -- TODO get rid of this eventually.
    , recoveryInProgress :: m Bool

    , securityParams     :: SecurityParams
    }

-- | The Monad constraint arises due to `transPipe` from Conduit.
--   The transformation function `foo :: (forall x. m x -> n x)` must be a
--   *monad morphism* and not just any natural transformation. This means,
--   roughly, that `foo a >> foo b` should behave the same as `foo (a >> b)`.
--   `foo = flip evalState 1`, for example, does not satisfy this requirement.
hoistLogic :: Monad m => (forall x . m x -> n x) -> Logic m -> Logic n
hoistLogic nat logic = logic
    { getSerializedBlock = nat . getSerializedBlock logic
    , streamBlocks = transPipe nat . streamBlocks logic
    , getBlockHeader = nat . getBlockHeader logic
    , getHashesRange = \a b c -> nat (getHashesRange logic a b c)
    , getBlockHeaders = \a b c -> nat (getBlockHeaders logic a b c)
    , getLcaMainChain = nat . getLcaMainChain logic
    , getTip = nat $ getTip logic
    , getTipHeader = nat $ getTipHeader logic
    , getAdoptedBVData = nat $ getAdoptedBVData logic
    , postBlockHeader = \a b -> nat (postBlockHeader logic a b)
    , postTx = hoistKeyVal nat (postTx logic)
    , postUpdate = hoistKeyVal nat (postUpdate logic)
    , postVote = hoistKeyVal nat (postVote logic)
    , postSscCommitment = hoistKeyVal nat (postSscCommitment logic)
    , postSscOpening = hoistKeyVal nat (postSscOpening logic)
    , postSscShares = hoistKeyVal nat (postSscShares logic)
    , postSscVssCert = hoistKeyVal nat (postSscVssCert logic)
    , postPskHeavy = nat . postPskHeavy logic
    , recoveryInProgress = nat $ recoveryInProgress logic
    }

-- | First iteration solution to the inv/req/data/mempool system.
-- Diffusion layer will set up the relays, but it needs help from the logic
-- layer in order to figure out what to request after an inv, what to relay
-- (the Bool return values), and of course what to actually do with the data,
-- which is tied up with whether it should be relayed (processing the data
-- and returning a Bool to indicate whether to relay).
--
--   - Does 'toKey' need to be monadic? Surely the datum itself contains the
--     key and this can be a pure function 'val -> key'. Maybe not? Must look
--     into it.
--   - How much help from the logic layer does the diffusion layer really need
--     in order to determine whether to relay something?
--     How much of the inv/req/data system can be done entirely by the
--     diffusion layer? Perhaps it should keep a little cache of keys it has
--     seen, ignore keys which are in that cache, and for keys which are not,
--     enqueue them up to the logic layer.
--     For handling 'Req' messages, though, logic layer support will in fact
--     be necessary I think; can't expect the diffusion layer to take care of
--     storing everything.
--
--   - Must investigate the mempool aspect.
--     Only transactions use this.
--     The MempoolParams KeyMempool constructor includes an m [key] which I
--     guess produces all keys in the mempool. Ah yes, this is so that a peer
--     can request everything in the mempool.
--     See 'handleMempoolL'. The entire mempool (as keys) is dumped to a list,
--     and then each key is 'Inv'd once (InvMsg sent).
--
--     I do not believe we ever make a mempool request (MempoolMsg).
--     Ok we can probably dump this.
data KeyVal key val m = KeyVal
    { toKey      :: val -> m key
    , handleInv  :: key -> m Bool
    , handleReq  :: key -> m (Maybe val)
    , handleData :: val -> m Bool
    }

hoistKeyVal :: (forall x . m x -> n x) -> KeyVal key val m -> KeyVal key val n
hoistKeyVal nat kv = kv
    { toKey = nat . toKey kv
    , handleInv = nat . handleInv kv
    , handleReq = nat . handleReq kv
    , handleData = nat . handleData kv
    }

dummyKeyVal :: Applicative m => KeyVal key val m
dummyKeyVal = KeyVal
    { toKey      = \_ -> error "dummy: can't make key"
    , handleInv  = \_ -> pure False
    , handleReq  = \_ -> pure Nothing
    , handleData = \_ -> pure False
    }

-- | A diffusion layer that does nothing, and probably crashes the program.
dummyLogic :: Monad m => Logic m
dummyLogic = Logic
    { ourStakeholderId   = error "dummy: no stakeholder id"
    , getSerializedBlock = \_ -> pure (error "dummy: can't get serialized block")
    , streamBlocks       = \_ -> pure ()
    , getBlockHeader     = \_ -> pure (error "dummy: can't get header")
    , getBlockHeaders    = \_ _ _ -> pure (error "dummy: can't get block headers")
    , getLcaMainChain    = \_ -> pure (NewestFirst [], OldestFirst [])
    , getHashesRange     = \_ _ _ -> pure (error "dummy: can't get hashes range")
    , getTip             = pure (error "dummy: can't get tip")
    , getTipHeader       = pure (error "dummy: can't get tip header")
    , getAdoptedBVData   = pure (error "dummy: can't get block version data")
    , postBlockHeader    = \_ _ -> pure ()
    , postPskHeavy       = \_ -> pure False
    , postTx             = dummyKeyVal
    , postUpdate         = dummyKeyVal
    , postVote           = dummyKeyVal
    , postSscCommitment  = dummyKeyVal
    , postSscOpening     = dummyKeyVal
    , postSscShares      = dummyKeyVal
    , postSscVssCert     = dummyKeyVal
    , recoveryInProgress = pure False
    , securityParams     = def
    }
