module Pos.Crypto.SignTag
       ( SignTag(..)
       , signTag
       ) where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Buildable
import           Formatting           (bprint, shown)

import qualified Pos.Binary.Class     as Bi
import           Pos.Core.Constants   (protocolMagic)

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
    -- | Transaction input:    @TxSigData@
    = SignTxIn
    -- | Vss certificate:      @(AsBinary VssPublicKey, EpochIndex)@
    | SignVssCert
    -- | Update proposal:      @UpdateProposalToSign@
    | SignUSProposal
    -- | Commitment:           @(EpochIndex, Commitment)@
    | SignCommitment
    -- | Update proposal vote: @(UpId, Bool)@
    | SignUSVote
    -- | Main block:           @MainToSign@
    | SignMainBlock
    deriving (Eq, Ord, Show, Generic, Typeable)

-- TODO: it would be nice if we couldn't use 'SignTag' with wrong
-- types. Maybe something with GADTs and data families?

instance Buildable SignTag where
    build = bprint shown

-- | Get magic bytes corresponding to a 'SignTag'. Guaranteed to be different
-- (and begin with a different byte) for different tags.
signTag :: SignTag -> ByteString
signTag = \case
    SignTxIn       -> "\x01" <> network
    SignVssCert    -> "\x02" <> network
    SignUSProposal -> "\x03" <> network
    SignCommitment -> "\x04" <> network
    SignUSVote     -> "\x05" <> network
    SignMainBlock  -> "\x06" <> network
  where
    network = BSL.toStrict (Bi.encode protocolMagic)
