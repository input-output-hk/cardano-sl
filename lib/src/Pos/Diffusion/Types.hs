{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Pos.Diffusion.Types
    ( DiffusionLayer (..)
    , Diffusion (..)
    , GetBlocksError (..)
    , dummyDiffusionLayer
    ) where

import           Universum
import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.Core.Block                   (Block, BlockHeader, MainBlockHeader)
import           Pos.Core.Types                   (HeaderHash)
import           Pos.Core.Txp                     (TxAux)
import           Pos.Core.Update                  (UpId, UpdateVote, UpdateProposal)
import           Pos.Ssc.Message                  (MCOpening, MCShares, MCCommitment,
                                                   MCVssCertificate)

-- | The interface to a diffusion layer, i.e. some component which takes care
-- of getting data in from and pushing data out to a network.
data Diffusion m = Diffusion
    { -- Get all blocks from a set of checkpoints to a given tip.
      getBlocks          :: NodeId
                         -> BlockHeader
                         -> [HeaderHash]
                         -> m (Either GetBlocksError [Block])
      -- This is needed because there's a security worker which will request
      -- tip-of-chain from the network if it determines it's very far behind.
      -- This type is chosen so that it fits with the current implementation:
      -- for each header received, dump it into the block retrieval queue and
      -- let the retrieval worker figure out all the recovery mode business.
    , requestTip         :: forall t . (BlockHeader -> NodeId -> m t) -> m (Map NodeId (m t))
      -- Announce a block.
    , announceBlock      :: MainBlockHeader -> m ()
      -- Returns a Bool iff at least one peer accepted the transaction.
      -- I believe it's for the benefit of wallets who wish to know that the
      -- transaction has a hope of making it into a block.
    , sendTx             :: TxAux -> m Bool
      -- Send an update proposal.
    , sendUpdateProposal :: UpId -> UpdateProposal -> [UpdateVote] -> m ()
      -- Send a vote for a proposal.
    , sendVote           :: UpdateVote -> m ()
      -- SSC: send our certificate (diffusion layer takes care of relaying
      -- certs for other stakeholders).
    , sendSscCert        :: MCVssCertificate -> m ()
      -- SSC: send our opening (diffusion layer takes care of relaying openings
      -- for other stakeholders).
    , sendSscOpening     :: MCOpening -> m ()
      -- SSC: send our shares (diffusion layer takes care of relaying shares
      -- for other stakeholders).
    , sendSscShares      :: MCShares -> m ()
      -- SSC: send our commitment (diffusion layer takes care of relaying
      -- commitments for other stakeholders).
    , sendSscCommitment  :: MCCommitment -> m ()
    }

-- | Failure description for getting blocks from the diffusion layer.
data GetBlocksError = GetBlocksError Text

deriving instance Show GetBlocksError

-- | A diffusion layer: its interface, and a way to run it.
data DiffusionLayer m = DiffusionLayer
    { runDiffusionLayer :: m ()
    , diffusion         :: Diffusion m
    }

-- | A diffusion layer that does nothing.
dummyDiffusionLayer :: Applicative m => DiffusionLayer m
dummyDiffusionLayer = DiffusionLayer
    { runDiffusionLayer = pure ()
    , diffusion         = dummyDiffusion
    }
  where
    dummyDiffusion :: Applicative m => Diffusion m
    dummyDiffusion = Diffusion
        { getBlocks          = \_ _ _ -> pure (Left (GetBlocksError "not implemented"))
        , requestTip         = \_ -> pure mempty
        , announceBlock      = \_ -> pure ()
        , sendTx             = \_ -> pure False
        , sendUpdateProposal = \_ _ _ -> pure ()
        , sendVote           = \_ -> pure ()
        , sendSscCert        = \_ -> pure ()
        , sendSscOpening     = \_ -> pure ()
        , sendSscShares      = \_ -> pure ()
        , sendSscCommitment  = \_ -> pure ()
        }
