-- | A pure logic layer. As such, it does nothing, but won't crash.

module Pos.Logic.Pure
    ( pureLogic
    , blockVersionData
    ) where

import           Universum

import qualified Data.ByteString as BS
import           Data.Coerce (coerce)

import           Pos.Binary.Class (serialize')
import           Pos.Chain.Block (Block, BlockHeader (..),
                     BlockHeaderAttributes, BlockSignature (..), HeaderHash,
                     MainBlock, MainBlockHeader, MainBody (..),
                     MainConsensusData (..), MainExtraBodyData (..),
                     MainExtraHeaderData (..), MainProof (..),
                     mkGenericBlockHeaderUnsafe, mkGenericBlockUnsafe)
import           Pos.Chain.Delegation (DlgPayload (..))
import           Pos.Chain.Ssc (SscPayload (..), SscProof (..),
                     VssCertificatesMap (..))
import           Pos.Chain.Txp (TxProof (..), emptyTxPayload)
import           Pos.Chain.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), SoftforkRule (..),
                     SoftwareVersion (..), UpdatePayload (..), UpdateProof)
import           Pos.Core (TxFeePolicy (..), unsafeCoinPortionFromDouble)
import           Pos.Core.Attributes (Attributes (..), UnparsedFields (..))
import           Pos.Core.Chrono (NewestFirst (..), OldestFirst (..))
import           Pos.Core.Common (BlockCount (..), ChainDifficulty (..))
import           Pos.Core.Merkle (MerkleRoot (..))
import           Pos.Core.Slotting (EpochIndex (..), LocalSlotIndex (..),
                     SlotId (..))
import           Pos.Crypto.Configuration (ProtocolMagic (..),
                     ProtocolMagicId (..), RequiresNetworkMagic (..))
import           Pos.Crypto.Hashing (Hash, unsafeMkAbstractHash)
import           Pos.Crypto.Signing (PublicKey (..), SecretKey (..),
                     Signature (..), deterministicKeyGen, signRaw)
import           Pos.DB.Class (Serialized (..), SerializedBlock)

import           Pos.Logic.Types (KeyVal (..), Logic (..))

-- | Serves up a single (invalid but well-formed) block and block header for
-- any request.
pureLogic
    :: ( Monad m )
    => Logic m
pureLogic = Logic
    { getSerializedBlock = \_ -> pure (Just serializedBlock)
    , streamBlocks       = \_ -> pure ()
    , getBlockHeader     = \_ -> pure (Just blockHeader)
    , getHashesRange     = \_ _ _ -> pure (Right (OldestFirst (pure mainBlockHeaderHash)))
    , getBlockHeaders    = \_ _ _ -> pure (Right (NewestFirst (pure blockHeader)))
      -- This definition of getLcaMainChain decides that all of the input
      -- hashes are *not* in the chain.
    , getLcaMainChain    = \hashes -> pure (NewestFirst [], hashes)
    , getTip             = pure block
    , getAdoptedBVData   = pure blockVersionData
    , postBlockHeader    = \_ _ -> pure ()
    , postPskHeavy       = \_ -> pure True
    , postTx             = dummyKeyVal
    , postUpdate         = dummyKeyVal
    , postVote           = dummyKeyVal
    , postSscCommitment  = dummyKeyVal
    , postSscOpening     = dummyKeyVal
    , postSscShares      = dummyKeyVal
    , postSscVssCert     = dummyKeyVal
    , recoveryInProgress = pure False
    }
  where
    dummyKeyVal :: Applicative m => KeyVal key val m
    dummyKeyVal = KeyVal
        { toKey      = \_ -> error "dummy: can't make key"
        , handleInv  = \_ -> pure False
        , handleReq  = \_ -> pure Nothing
        , handleData = \_ -> pure False
        }

blockVersionData :: BlockVersionData
blockVersionData = BlockVersionData
    { bvdScriptVersion = 0
    , bvdSlotDuration  = 0
    -- FIXME
    -- Unfortunately, the choices we make here will affect the functioning of
    -- the diffusion layer.
    -- There's no way to say "no limit".
    --
    -- To fix this, perhaps we should augment the logic layer interface with
    --
    --   limits :: m Limits
    --
    -- where Limits gives all of these 4 limits. Then we can use them to do
    -- limiting at the value level, rather than how it is now, via type classes.
    , bvdMaxBlockSize    = limit
    , bvdMaxHeaderSize   = limit
    , bvdMaxTxSize       = limit
    , bvdMaxProposalSize = limit

    , bvdMpcThd            = unsafeCoinPortionFromDouble 0
    , bvdHeavyDelThd       = unsafeCoinPortionFromDouble 0
    , bvdUpdateVoteThd     = unsafeCoinPortionFromDouble 0
    , bvdUpdateProposalThd = unsafeCoinPortionFromDouble 0

    , bvdUpdateImplicit = 0

    , bvdSoftforkRule     = SoftforkRule
          { srInitThd      = unsafeCoinPortionFromDouble 0
          , srMinThd       = unsafeCoinPortionFromDouble 0
          , srThdDecrement = unsafeCoinPortionFromDouble 0
          }
    , bvdTxFeePolicy      = TxFeePolicyUnknown 0 mempty
    , bvdUnlockStakeEpoch = EpochIndex { getEpochIndex = 0 }
    }
  where
    limit = fromIntegral ((2 :: Int) ^ (32 :: Int))

-- What follows is the definition of a block.
-- It's not a *valid* block, but that's OK! I just want a well-formed block
-- which this pure logic layer can give out for every block request (getBlock,
-- getTip), along with a well-formed block header to give for every header
-- request.
-- The diffusion layer should work just fine even if the logic layer gives
-- *invalid* but *well-formed* blocks.

-- | This block is always given by 'getBlock' and 'getTip'
block :: Block
block = Right mainBlock

serializedBlock :: SerializedBlock
serializedBlock = Serialized $ serialize' block

mainBlock :: MainBlock
mainBlock = mkGenericBlockUnsafe mainBlockHeader blockBody extraBodyData

blockBody :: MainBody
blockBody = MainBody
    { _mbTxPayload     = emptyTxPayload
    , _mbSscPayload    = emptySscPayload
    , _mbDlgPayload    = emptyDlgPayload
    , _mbUpdatePayload = emptyUpdatePayload
    }

-- SscPayload is 4 alternatives. I chose CertificatesPayload because it has
-- the fewest fields...
emptySscPayload :: SscPayload
emptySscPayload = CertificatesPayload
    (UnsafeVssCertificatesMap
          { getVssCertificatesMap = mempty
          }
    )

emptyDlgPayload :: DlgPayload
emptyDlgPayload = UnsafeDlgPayload
    { getDlgPayload = mempty
    }

emptyUpdatePayload :: UpdatePayload
emptyUpdatePayload = UpdatePayload
    { upProposal = Nothing
    , upVotes    = []
    }

extraBodyData :: MainExtraBodyData
extraBodyData = MainExtraBodyData
    { _mebAttributes = Attributes
          { attrData   = ()
          , attrRemain = UnparsedFields mempty
          }
    }

blockHeader :: BlockHeader
blockHeader = BlockHeaderMain mainBlockHeader

mainBlockHeader :: MainBlockHeader
mainBlockHeader = mkGenericBlockHeaderUnsafe
    protocolMagic
    mainBlockHeaderHash
    bodyProof
    consensusData
    extraHeaderData

mainBlockHeaderHash :: HeaderHash
mainBlockHeaderHash = unsafeMkAbstractHash mempty

bodyProof :: MainProof
bodyProof = MainProof
    { mpTxProof       = txProof
    , mpMpcProof      = sscProof
    , mpProxySKsProof = dlgProof
    , mpUpdateProof   = updateProof
    }

txProof :: TxProof
txProof = TxProof
    { txpNumber        = 0
    , txpRoot          = MerkleRoot (unsafeMkAbstractHash mempty)
    , txpWitnessesHash = unsafeMkAbstractHash mempty
    }

sscProof :: SscProof
sscProof = CertificatesProof
    (unsafeMkAbstractHash mempty)

dlgProof :: Hash DlgPayload
dlgProof = unsafeMkAbstractHash mempty

updateProof :: UpdateProof
updateProof = unsafeMkAbstractHash mempty

consensusData :: MainConsensusData
consensusData = MainConsensusData
    { _mcdSlot       = slotId
    , _mcdLeaderKey  = publicKey
    , _mcdDifficulty = chainDifficulty
    , _mcdSignature  = blockSignature
    }

slotId :: SlotId
slotId = SlotId
    { siEpoch = EpochIndex { getEpochIndex = 0 }
    , siSlot  = UnsafeLocalSlotIndex { getSlotIndex = 0 }
    }

-- Trivia: the seed has to be at least 32 bytes.
publicKey :: PublicKey
secretKey :: SecretKey
(publicKey, secretKey) = deterministicKeyGen (BS.pack (replicate 32 0))

chainDifficulty :: ChainDifficulty
chainDifficulty = ChainDifficulty
    { getChainDifficulty = BlockCount { getBlockCount = 0 }
    }

blockSignature :: BlockSignature
blockSignature = BlockSignature (coerce (signRaw protocolMagic Nothing secretKey mempty))

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 0

protocolMagic :: ProtocolMagic
protocolMagic = ProtocolMagic protocolMagicId RequiresNoMagic

extraHeaderData :: MainExtraHeaderData
extraHeaderData = MainExtraHeaderData
    { _mehBlockVersion    = blockVersion
    , _mehSoftwareVersion = softwareVersion
    , _mehAttributes      = blockHeaderAttributes
    , _mehEBDataProof     = extraBodyDataProof
    }

blockVersion :: BlockVersion
blockVersion = BlockVersion
    { bvMajor = 0
    , bvMinor = 0
    , bvAlt   = 0
    }

softwareVersion :: SoftwareVersion
softwareVersion = SoftwareVersion
    { svAppName = appName
    , svNumber  = 0
    }
  where
    appName = ApplicationName (mempty :: Text)

blockHeaderAttributes :: BlockHeaderAttributes
blockHeaderAttributes = Attributes
    { attrData   = ()
    , attrRemain = UnparsedFields mempty
    }

extraBodyDataProof :: Hash MainExtraBodyData
extraBodyDataProof = unsafeMkAbstractHash mempty
