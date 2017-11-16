{-# LANGUAGE StandaloneDeriving #-}

module Pos.Logic.Types
    ( LogicLayer (..)
    , Logic (..)
    , KeyVal (..)
    , GetBlockError (..)
    , GetBlockHeaderError (..)
    , GetTipError (..)
    , dummyLogicLayer
    ) where

import           Universum
import           Pos.Communication         (NodeId)
import           Pos.Core.Block            (Block, BlockHeader)
import           Pos.Core.Types            (HeaderHash, StakeholderId,
                                            ProxySKHeavy)
import           Pos.Core.Txp              (TxId, TxAux)
import           Pos.Core.Update           (UpId, UpdateVote, UpdateProposal)
import           Pos.Ssc.Message           (MCOpening, MCShares, MCCommitment,
                                            MCVssCertificate)

-- | The interface to a diffusion layer, i.e. some component which takes care
-- of getting data in from and pushing data out to a network.
data Logic m = Logic
    { -- The stakeholder id of our node.
      ourStakeholderId   :: StakeholderId
      -- Get a block, perhaps from a database.
    , getBlock           :: HeaderHash -> m (Either GetBlockError (Maybe Block))
      -- Get a block header.
      -- TBD: necessary? Is it any different/faster than getting the block
      -- and taking the header?
    , getBlockHeader     :: HeaderHash -> m (Either GetBlockHeaderError (Maybe BlockHeader))
      -- Get the current tip of chain.
      -- It's not in Maybe, as getBlock is, because really there should always
      -- be a tip, whereas trying to get a block that isn't in the database is
      -- normal.
    , getTip             :: m (Either GetTipError Block)

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
    , postTx            :: KeyVal TxId TxAux m
    , postUpdate        :: KeyVal UpId (UpdateProposal, [UpdateVote]) m
    , postSscCommitment :: KeyVal StakeholderId MCCommitment m
    , postSscOpening    :: KeyVal StakeholderId MCOpening m
    , postSscShares     :: KeyVal StakeholderId MCShares m
    , postSscVssCert    :: KeyVal StakeholderId MCVssCertificate m

      -- Give a heavy delegation certificate. Returns False if something
      -- went wrong.
      --
      -- NB light delegation is apparently disabled in master.
    , postPskHeavy      :: ProxySKHeavy -> m Bool
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

-- | Failure description for getting the tip of chain from the logic layer.
data GetTipError = GetTipError Text

deriving instance Show GetTipError
instance Exception GetTipError

-- | A diffusion layer: its interface, and a way to run it.
data LogicLayer m = LogicLayer
    { runLogicLayer :: m ()
    , logic         :: Logic m
    }

-- | A diffusion layer that does nothing, and probably crahes the program.
dummyLogicLayer :: Applicative m => StakeholderId -> LogicLayer m
dummyLogicLayer stkhldId = LogicLayer
    { runLogicLayer = pure ()
    , logic         = dummyLogic
    }

  where

    dummyLogic :: Applicative m => Logic m
    dummyLogic = Logic
        { ourStakeholderId  = stkhldId
        , getBlock          = \_ -> pure (Right Nothing)
        , getBlockHeader    = \_ -> pure (Right Nothing)
        , getTip            = pure (Left (GetTipError "dummy: no tip"))
        , postBlockHeader   = \_ _ -> pure ()
        , postPskHeavy      = \_ -> pure False
        , postTx            = dummyKeyVal
        , postUpdate        = dummyKeyVal
        , postSscCommitment = dummyKeyVal
        , postSscOpening    = dummyKeyVal
        , postSscShares     = dummyKeyVal
        , postSscVssCert    = dummyKeyVal
        }

    dummyKeyVal :: Applicative m => KeyVal key val m
    dummyKeyVal = KeyVal
        { toKey      = \_ -> error "dummy: can't make key"
        , handleInv  = \_ -> pure False
        , handleReq  = \_ -> pure Nothing
        , handleData = \_ -> pure False
        }
