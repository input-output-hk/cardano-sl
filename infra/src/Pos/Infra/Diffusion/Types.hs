{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pos.Infra.Diffusion.Types
    ( DiffusionLayer (..)
    , Diffusion (..)
    , hoistDiffusion
    , dummyDiffusionLayer
    , DiffusionHealth (..)
    ) where

import           Universum

import           Data.Map.Strict (Map)
import           Formatting (Format, stext)
import           System.Metrics.Gauge (Gauge)

import           Pos.Core.Block (Block, BlockHeader, HeaderHash,
                     MainBlockHeader)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.Delegation (ProxySKHeavy)
import           Pos.Core.Ssc (InnerSharesMap, Opening, SignedCommitment,
                     VssCertificate)
import           Pos.Core.Txp (TxAux)
import           Pos.Core.Update (UpId, UpdateProposal, UpdateVote)
import           Pos.Infra.Communication.Types.Protocol (NodeId)
import           Pos.Infra.Diffusion.Subscription.Status (SubscriptionStates,
                     emptySubscriptionStates)
import           Pos.Infra.Reporting.Health.Types (HealthStatus (..))


data DiffusionHealth = DiffusionHealth {
    dhStreamWriteQueue :: !Gauge -- Number of blocks stored in the block stream write queue
  , dhStreamWindow     :: !Gauge -- Current Stream Window size.
  }

-- | The interface to a diffusion layer, i.e. some component which takes care
-- of getting data in from and pushing data out to a network.
data Diffusion m = Diffusion
    { -- | Get all blocks from a set of checkpoints to a given tip.
      -- The blocks come in oldest first, and form a chain (prev header of
      -- {n}'th is the header of {n-1}th.
      getBlocks          :: NodeId
                         -> HeaderHash
                         -> [HeaderHash]
                         -> m (OldestFirst [] Block)
    , streamBlocks       :: forall t .
                            NodeId
                         -> HeaderHash
                         -> [HeaderHash]
                         -> ([Block] -> m t)
                         -> m (Maybe t)
      -- | This is needed because there's a security worker which will request
      -- tip-of-chain from the network if it determines it's very far behind.
    , requestTip          :: m (Map NodeId (m BlockHeader))
      -- | Announce a block header.
    , announceBlockHeader :: MainBlockHeader -> m ()
      -- | Returns a Bool iff at least one peer accepted the transaction.
      -- I believe it's for the benefit of wallets who wish to know that the
      -- transaction has a hope of making it into a block.
    , sendTx             :: TxAux -> m Bool
      -- | Send an update proposal.
    , sendUpdateProposal :: UpId -> UpdateProposal -> [UpdateVote] -> m ()
      -- | Send a vote for a proposal.
    , sendVote           :: UpdateVote -> m ()
      -- | SSC: send our certificate (diffusion layer takes care of relaying
      -- certs for other stakeholders).
    , sendSscCert        :: VssCertificate -> m ()
      -- | SSC: send our opening (diffusion layer takes care of relaying openings
      -- for other stakeholders).
    , sendSscOpening     :: Opening -> m ()
      -- | SSC: send our shares (diffusion layer takes care of relaying shares
      -- for other stakeholders).
    , sendSscShares      :: InnerSharesMap -> m ()
      -- | SSC: send our commitment (diffusion layer takes care of relaying
      -- commitments for other stakeholders).
    , sendSscCommitment  :: SignedCommitment -> m ()
      -- | Delegation: send a heavy certificate.
    , sendPskHeavy       :: ProxySKHeavy -> m ()

      -- | FIXME stopgap measure: there's an amazon route53 health check server
      -- that we have to support. Also a reporting mechanism that still
      -- demands to know the current set of peers we may be talking to.
      -- In the future we should roll this in with a more general status/debug
      -- system: to be used by the reporting mechanism (supply info about
      -- network topology) and also by the user interface (how's our connection
      -- quality?). [CSL-2147]
    , healthStatus       :: m HealthStatus
      -- | For debugging/reporting purposes.
    , formatStatus       :: forall r . (forall a . Format r a -> a) -> m r
      -- | Subscriptin statuses to all nodes.  If the node is not subscribed it
      -- is not in the map.
    , subscriptionStates :: SubscriptionStates NodeId
    }

-- | A diffusion layer: its interface, and a way to run it.
data DiffusionLayer m = DiffusionLayer
    { runDiffusionLayer :: forall x . m x -> m x
    , diffusion         :: Diffusion m
    }

hoistDiffusion
    :: Functor m
    => (forall t . m t -> n t)
    -> (forall t . n t -> m t)
    -> Diffusion m
    -> Diffusion n
hoistDiffusion nat rnat orig = Diffusion
    { getBlocks = \nid bh hs -> nat $ getBlocks orig nid bh hs
    , streamBlocks = \nid hh hhs k -> nat $ streamBlocks orig nid hh hhs (rnat . k)
    , requestTip = nat $ (fmap . fmap) nat (requestTip orig)
    , announceBlockHeader = nat . announceBlockHeader orig
    , sendTx = nat . sendTx orig
    , sendUpdateProposal = \upid upp upvs -> nat $ sendUpdateProposal orig upid upp upvs
    , sendVote = nat . sendVote orig
    , sendSscCert = nat . sendSscCert orig
    , sendSscOpening = nat . sendSscOpening orig
    , sendSscShares = nat . sendSscShares orig
    , sendSscCommitment = nat . sendSscCommitment orig
    , sendPskHeavy = nat . sendPskHeavy orig
    , healthStatus = nat $ healthStatus orig
    , formatStatus = \fmt -> nat $ formatStatus orig fmt
    , subscriptionStates = subscriptionStates orig
    }

-- | A diffusion layer that does nothing.
dummyDiffusionLayer :: Applicative d => IO (DiffusionLayer d)
dummyDiffusionLayer = do
    ss <- emptySubscriptionStates
    return DiffusionLayer
        { runDiffusionLayer = identity
        , diffusion         = dummyDiffusion ss
        }
  where
    dummyDiffusion :: Applicative m => SubscriptionStates NodeId -> Diffusion m
    dummyDiffusion subscriptionStates = Diffusion
        { getBlocks          = \_ _ _ -> pure (OldestFirst [])
        , requestTip         = pure mempty
        , streamBlocks        = \_ _ _ _ -> pure Nothing
        , announceBlockHeader = \_ -> pure ()
        , sendTx             = \_ -> pure True
        , sendUpdateProposal = \_ _ _ -> pure ()
        , sendVote           = \_ -> pure ()
        , sendSscCert        = \_ -> pure ()
        , sendSscOpening     = \_ -> pure ()
        , sendSscShares      = \_ -> pure ()
        , sendSscCommitment  = \_ -> pure ()
        , sendPskHeavy       = \_ -> pure ()
        , healthStatus       = pure (HSUnhealthy "I'm a dummy")
        , formatStatus       = \fmt -> pure (fmt stext "")
        , ..
        }
