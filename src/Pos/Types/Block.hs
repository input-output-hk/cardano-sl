-- | Block related functions.

module Pos.Types.Block
       ( mkBlockHeader

       , verifyHeader
       , verifySignedHeader
       ) where

import           Data.Binary          (Binary)
import           Formatting           (build, sformat, (%))
import           Serokell.Util.Verify (VerificationRes (..), verifyGeneric)
import           Universum

import           Pos.Crypto           (PublicKey, hash, unsafeHash, verify)
import           Pos.Types.Types      (BlockHeader (..), ChainDifficulty, CommitmentsMap,
                                       HeaderHash, OpeningsMap, SignedBlockHeader (..),
                                       SlotId)

genesisHash :: HeaderHash p
genesisHash = unsafeHash ("patak" :: Text)

hashPrev :: Binary p => Maybe (SignedBlockHeader p) -> HeaderHash p
hashPrev = maybe genesisHash hash

mkBlockHeader
    :: Binary p
    => Maybe (SignedBlockHeader p)
    -> SlotId
    -> PublicKey
    -> CommitmentsMap
    -> OpeningsMap
    -> ChainDifficulty
    -> p
    -> BlockHeader p
mkBlockHeader prevHeader slotId pk comms opens difficulty proof =
    BlockHeader
    { bhPrevHash = maybe genesisHash hash prevHeader
    , bhSlot = slotId
    , bhLeaderKey = pk
    , bhCommitments = comms
    , bhOpenings = opens
    , bhDifficulty = difficulty
    , bhPayloadProof = proof
    }

-- | Perform cheap checks of BlockHeader, which can be done using only
-- header itself and previous header.
-- TODO: extend.
verifyHeader
    :: Binary p
    => Maybe (SignedBlockHeader p) -> BlockHeader p -> VerificationRes
verifyHeader prevHeader BlockHeader {..} =
    verifyGeneric
        [ ( bhPrevHash == prevHash
          , sformat
                ("inconsistent previous hash (expected "%build% ", found"%build%")")
                prevHash bhPrevHash)
        ]
  where
    prevHash = hashPrev prevHeader

verifySignedHeader
    :: Binary p
    => Maybe (SignedBlockHeader p) -> SignedBlockHeader p -> VerificationRes
verifySignedHeader prevHeader SignedBlockHeader {..} =
    mconcat
        [ verifyGeneric
              [ ( verify (bhLeaderKey sbhHeader) sbhHeader sbhSignature
                , "signature is incorrect")
              ]
        , verifyHeader prevHeader sbhHeader
        ]
