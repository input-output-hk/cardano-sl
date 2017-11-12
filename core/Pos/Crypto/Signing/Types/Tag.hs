module Pos.Crypto.Signing.Types.Tag
       ( SignTag(..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, shown)

-- | To protect agains replay attacks (i.e. when an attacker intercepts a
-- signed piece of data and later sends it again), we add a tag to all data
-- that we sign. This ensures that even if some bytestring can be
-- deserialized into two different types of messages (A and B), the attacker
-- can't take message A and send it as message B.
--
-- We also automatically add the network tag ('protocolMagic') whenever it
-- makes sense, to ensure that things intended for testnet won't work for
-- mainnet.
data SignTag
    = SignForTestingOnly  -- ^ Anything (to be used for testing only)
    | SignTx              -- ^ Tx:               @TxSigData@
    | SignRedeemTx        -- ^ Redeem tx:        @TxSigData@
    | SignVssCert         -- ^ Vss certificate:  @(VssPublicKey, EpochIndex)@
    | SignUSProposal      -- ^ Update proposal:  @UpdateProposalToSign@
    | SignCommitment      -- ^ Commitment:       @(EpochIndex, Commitment)@
    | SignUSVote          -- ^ US proposal vote: @(UpId, Bool)@
    | SignMainBlock       -- ^ Main block:       @MainToSign@
    | SignMainBlockLight
    | SignMainBlockHeavy
    | SignProxySK         -- ^ Proxy key:        @ProxySecretKey@
    deriving (Eq, Ord, Show, Generic, Typeable)

-- TODO: it would be nice if we couldn't use 'SignTag' with wrong
-- types. Maybe something with GADTs and data families?

instance Buildable SignTag where
    build = bprint shown
