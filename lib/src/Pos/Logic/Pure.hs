-- | A pure logic layer. As such, it does nothing, but won't crash.

module Pos.Logic.Pure
    ( pureLogic
    , blockVersionData
    ) where

import           Universum

import           Data.Default (def)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Cardano.Crypto.Wallet (xpub, xsignature)
import qualified Crypto.Hash as Crypto (hash)

import           Pos.Core (StakeholderId, SoftwareVersion (..), BlockVersion (..),
                           HeaderHash, Block, BlockHeader, GenericBlock (..),
                           GenericBlockHeader (..), ExtraHeaderData, ExtraBodyData,
                           BlockVersionData (..), ApplicationName, mkApplicationName,
                           unsafeCoinPortionFromDouble, TxFeePolicy (..), SoftforkRule (..))
import           Pos.Core.Block.Main
import           Pos.Core.Common (ChainDifficulty (..), BlockCount (..))
import           Pos.Core.Delegation (DlgPayload (..))
import           Pos.Core.Ssc (SscPayload (..), SscProof (..), VssCertificatesMap (..))
import           Pos.Core.Slotting (SlotId (..), EpochIndex (..), LocalSlotIndex (..))
import           Pos.Core.Txp (TxPayload (..), TxProof (..))
import           Pos.Core.Update (UpdatePayload (..), UpdateProof)
import           Pos.Crypto.Hashing (AbstractHash (..), Hash)
import           Pos.Crypto.Signing (PublicKey (..), Signature (..))
import           Pos.Data.Attributes (UnparsedFields (..), Attributes (..))
import           Pos.Merkle (MerkleTree (..), MerkleRoot (..))
import           Pos.Util.Chrono (OldestFirst (..), NewestFirst (..))

import           Pos.Logic.Types (Logic (..), KeyVal (..))

-- | Serves up a single (invalid but well-formed) block and block header for
-- any request.
pureLogic
    :: ( Applicative m )
    => Logic m
pureLogic = Logic
    { ourStakeholderId   = stakeholderId
    , getBlock           = \_ -> pure (Right (Just block))
    , getBlockHeader     = \_ -> pure (Right (Just blockHeader))
    , getBlockHeaders    = \_ _ -> pure (Right (NewestFirst (pure blockHeader)))
    , getBlockHeaders'   = \_ _ -> pure (Right (Just (OldestFirst (pure mainBlockHeaderHash))))
    , getTip             = pure (Right block)
    , getTipHeader       = pure (Right blockHeader)
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
    , securityParams     = def
    , checkBlockHeader   = \_ -> pure True
    , checkBlock         = \_ -> pure True
    }
  where
    dummyKeyVal :: Applicative m => KeyVal key val m
    dummyKeyVal = KeyVal
        { toKey      = \_ -> error "dummy: can't make key"
        , handleInv  = \_ -> pure False
        , handleReq  = \_ -> pure Nothing
        , handleData = \_ -> pure False
        }

stakeholderId :: StakeholderId
stakeholderId = AbstractHash (Crypto.hash (mempty :: ByteString))

{-
data BlockVersionData = BlockVersionData
    { bvdScriptVersion     :: !ScriptVersion
    , bvdSlotDuration      :: !Millisecond
    , bvdMaxBlockSize      :: !Byte
    , bvdMaxHeaderSize     :: !Byte
    , bvdMaxTxSize         :: !Byte
    , bvdMaxProposalSize   :: !Byte
    , bvdMpcThd            :: !CoinPortion
    , bvdHeavyDelThd       :: !CoinPortion
    , bvdUpdateVoteThd     :: !CoinPortion
    , bvdUpdateProposalThd :: !CoinPortion
    , bvdUpdateImplicit    :: !FlatSlotId
    , bvdSoftforkRule      :: !SoftforkRule
    , bvdTxFeePolicy       :: !TxFeePolicy
    , bvdUnlockStakeEpoch  :: !EpochIndex
    } deriving (Show, Eq, Ord, Generic, Typeable)
-}

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

-- type Block = Either GenesisBlock MainBlock
-- type MainBlock = GenericBlock MainBlockChain
--
-- data GenericBlock b = UnsafeGenericBlock
--     { _gbHeader :: !(GenericBlockHeader b)
--     , _gbBody   :: !(Body b)
--     , _gbExtra  :: !(ExtraBodyData b)
--     } deriving (Generic)
--
-- data GenericBlockHeader b = UnsafeGenericBlockHeader
--     { -- | Pointer to the header of the previous block.
--       _gbhPrevBlock :: !(BHeaderHash b)
--     , -- | Proof of body.
--       _gbhBodyProof :: !(BodyProof b)
--     , -- | Consensus data to verify consensus algorithm.
--       _gbhConsensus :: !(ConsensusData b)
--     , -- | Any extra data.
--       _gbhExtra     :: !(ExtraHeaderData b)
--     } deriving (Generic)


-- | This block is always given by 'getBlock' and 'getTip'
block :: Block
block = Right mainBlock

mainBlock :: MainBlock
mainBlock = UnsafeGenericBlock
    { _gbHeader = mainBlockHeader
    , _gbBody   = blockBody
    , _gbExtra  = extraBodyData
    }

blockBody :: Body MainBlockchain
blockBody = MainBody
    { _mbTxPayload     = emptyTxPayload
    , _mbSscPayload    = emptySscPayload
    , _mbDlgPayload    = emptyDlgPayload
    , _mbUpdatePayload = emptyUpdatePayload
    }

emptyTxPayload :: TxPayload
emptyTxPayload = UnsafeTxPayload
    { _txpTxs       = MerkleEmpty
    , _txpWitnesses = []
    }

-- SscPayload is 4 alternatives. I chose CertificatesPayload because it has
-- the fewest fields...
emptySscPayload :: SscPayload
emptySscPayload = CertificatesPayload
    { spVss = UnsafeVssCertificatesMap
          { getVssCertificatesMap = mempty
          }
    }

emptyDlgPayload :: DlgPayload
emptyDlgPayload = UnsafeDlgPayload
    { getDlgPayload = []
    }

emptyUpdatePayload :: UpdatePayload
emptyUpdatePayload = UpdatePayload
    { upProposal = Nothing
    , upVotes    = []
    }

extraBodyData :: ExtraBodyData MainBlockchain
extraBodyData = MainExtraBodyData
    { _mebAttributes = Attributes
          { attrData   = ()
          , attrRemain = UnparsedFields mempty
          }
    }

blockHeader :: BlockHeader
blockHeader = Right mainBlockHeader

mainBlockHeader :: MainBlockHeader
mainBlockHeader = UnsafeGenericBlockHeader
    { _gbhPrevBlock = mainBlockHeaderHash
    , _gbhBodyProof = bodyProof
    , _gbhConsensus = consensusData
    , _gbhExtra     = extraHeaderData
    }

mainBlockHeaderHash :: HeaderHash
mainBlockHeaderHash = AbstractHash (Crypto.hash (mempty :: ByteString))

bodyProof :: BodyProof MainBlockchain
bodyProof = MainProof
    { mpTxProof       = txProof
    , mpMpcProof      = sscProof
    , mpProxySKsProof = dlgProof
    , mpUpdateProof   = updateProof
    }

txProof :: TxProof
txProof = TxProof
    { txpNumber        = 0
    , txpRoot          = MerkleRoot (AbstractHash (Crypto.hash (mempty :: ByteString)))
    , txpWitnessesHash = AbstractHash (Crypto.hash (mempty :: ByteString))
    }

sscProof :: SscProof
sscProof = CertificatesProof
    { sprVss = AbstractHash (Crypto.hash (mempty :: ByteString))
    }

dlgProof :: Hash DlgPayload
dlgProof = AbstractHash (Crypto.hash (mempty :: ByteString))

updateProof :: UpdateProof
updateProof = AbstractHash (Crypto.hash (mempty :: ByteString))

consensusData :: ConsensusData MainBlockchain
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

publicKey :: PublicKey
publicKey = PublicKey key
  where
    Right key = xpub (BS.replicate 64 0)

chainDifficulty :: ChainDifficulty
chainDifficulty = ChainDifficulty
    { getChainDifficulty = BlockCount { getBlockCount = 0 }
    }

blockSignature :: BlockSignature
blockSignature = BlockSignature (Signature sig)
  where
    Right sig = xsignature (BS.replicate 64 0)

extraHeaderData :: ExtraHeaderData MainBlockchain
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
    Right appName = (mkApplicationName (mempty :: Text) :: Either String ApplicationName)

blockHeaderAttributes :: BlockHeaderAttributes
blockHeaderAttributes = Attributes
    { attrData   = ()
    , attrRemain = UnparsedFields mempty
    }

extraBodyDataProof :: Hash MainExtraBodyData
extraBodyDataProof = AbstractHash (Crypto.hash (mempty :: ByteString))
