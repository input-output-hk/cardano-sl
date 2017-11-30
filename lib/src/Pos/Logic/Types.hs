{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pos.Logic.Types
    ( LogicLayer (..)
    , Logic (..)
    , KeyVal (..)
    , GetBlockError (..)
    , GetBlockHeaderError (..)
    , GetBlockHeadersError (..)
    , GetTipError (..)
    , dummyLogicLayer
    ) where

import           Universum
import           Data.Default              (def)
import           Data.Tagged               (Tagged)

import           Pos.Communication         (NodeId, TxMsgContents)
import           Pos.Core.Block            (Block, BlockHeader)
import           Pos.Core.Types            (HeaderHash, StakeholderId,
                                            ProxySKHeavy)
import           Pos.Core.Txp              (TxId)
import           Pos.Core.Update           (UpId, UpdateVote, UpdateProposal, BlockVersionData)
import           Pos.Security              (SecurityParams (..))
import           Pos.Ssc.Message           (MCOpening, MCShares, MCCommitment,
                                            MCVssCertificate)
import           Pos.Util.Chrono           (NewestFirst, OldestFirst, NE)

-- | The interface to a logic layer, i.e. some component which encapsulates
-- blockchain / crypto logic.
data Logic m = Logic
    { -- The stakeholder id of our node.
      ourStakeholderId   :: StakeholderId
      -- Get a block, perhaps from a database.
    , getBlock           :: HeaderHash -> m (Either GetBlockError (Maybe Block))
      -- Get a block header.
      -- TBD: necessary? Is it any different/faster than getting the block
      -- and taking the header?
    , getBlockHeader     :: HeaderHash -> m (Either GetBlockHeaderError (Maybe BlockHeader))
      -- Inspired by 'getHeadersFromManyTo'.
      -- Included here because that function is quite complicated; it's not
      -- clear whether it can be expressed simply in terms of getBlockHeader.:q
    , getBlockHeaders    :: NonEmpty HeaderHash -> Maybe HeaderHash -> m (Either GetBlockHeadersError (NewestFirst NE BlockHeader))
      -- Inspired by 'getHeadersFromToIncl', which is apparently distinct from
      -- 'getHeadersFromManyTo' (getBlockHeaders without the tick above).
      -- FIXME we must unify these.
      -- May want to think about giving a streaming-IO interface (pipes, conduit
      -- or similar).
    , getBlockHeaders'   :: HeaderHash -> HeaderHash -> m (Either GetBlockHeadersError (Maybe (OldestFirst NE HeaderHash)))
      -- Get the current tip of chain.
      -- It's not in Maybe, as getBlock is, because really there should always
      -- be a tip, whereas trying to get a block that isn't in the database is
      -- normal.
    , getTip             :: m (Either GetTipError Block)

      -- | Get state of last adopted BlockVersion. Related to update system.
    , getAdoptedBVData   :: m BlockVersionData

      -- Give a block header to the logic layer.
      -- NodeId is needed for first iteration, but will be removed later.
    , postBlockHeader    :: BlockHeader -> NodeId -> m ()

      -- Tx, update, ssc... 
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
    , postTx            :: KeyVal (Tagged TxMsgContents TxId) TxMsgContents m
    , postUpdate        :: KeyVal (Tagged (UpdateProposal, [UpdateVote]) UpId) (UpdateProposal, [UpdateVote]) m
    , postVote          :: KeyVal (Tagged UpdateVote UpId) UpdateVote m
    , postSscCommitment :: KeyVal (Tagged MCCommitment StakeholderId) MCCommitment m
    , postSscOpening    :: KeyVal (Tagged MCOpening StakeholderId) MCOpening m
    , postSscShares     :: KeyVal (Tagged MCShares StakeholderId) MCShares m
    , postSscVssCert    :: KeyVal (Tagged MCVssCertificate StakeholderId) MCVssCertificate m

      -- Give a heavy delegation certificate. Returns False if something
      -- went wrong.
      --
      -- NB light delegation is apparently disabled in master.
    , postPskHeavy      :: ProxySKHeavy -> m Bool

      -- Recovery mode related stuff.
      -- TODO get rid of this eventually.
    , recoveryInProgress :: m Bool

    , securityParams     :: SecurityParams
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
    { toKey :: val -> m key
    , handleInv :: key -> m Bool
    , handleReq :: key -> m (Maybe val)
    , handleData :: val -> m Bool
    }

-- | Failure description for getting a block from the logic layer.
data GetBlockError = GetBlockError Text

deriving instance Show GetBlockError
instance Exception GetBlockError

-- | Failure description for getting a block header from the logic layer.
data GetBlockHeaderError = GetBlockHeaderError Text

deriving instance Show GetBlockHeaderError
instance Exception GetBlockHeaderError

-- | Failure description for getting a block header from the logic layer.
data GetBlockHeadersError = GetBlockHeadersError Text

deriving instance Show GetBlockHeadersError
instance Exception GetBlockHeadersError

-- | Failure description for getting the tip of chain from the logic layer.
data GetTipError = GetTipError Text

deriving instance Show GetTipError
instance Exception GetTipError

-- | A diffusion layer: its interface, and a way to run it.
data LogicLayer m = LogicLayer
    { runLogicLayer :: forall x . m x -> m x
    , logic         :: Logic m
    }

-- | A diffusion layer that does nothing, and probably crahes the program.
dummyLogicLayer
    :: ( Applicative m )
    => LogicLayer m
dummyLogicLayer = LogicLayer
    { runLogicLayer = identity
    , logic         = dummyLogic
    }

  where

    dummyLogic :: Applicative m => Logic m
    dummyLogic = Logic
        { ourStakeholderId   = error "dummy: no stakeholder id"
        , getBlock           = \_ -> pure (error "dummy: can't get block")
        , getBlockHeader     = \_ -> pure (error "dummy: can't get header")
        , getBlockHeaders    = \_ _ -> pure (error "dummy: can't get headers")
        , getBlockHeaders'   = \_ _ -> pure (error "dummy: can't get headers")
        , getTip             = pure (error "dummy: can't get tip")
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

    dummyKeyVal :: Applicative m => KeyVal key val m
    dummyKeyVal = KeyVal
        { toKey      = \_ -> error "dummy: can't make key"
        , handleInv  = \_ -> pure False
        , handleReq  = \_ -> pure Nothing
        , handleData = \_ -> pure False
        }
