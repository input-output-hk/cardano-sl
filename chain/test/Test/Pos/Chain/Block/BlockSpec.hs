{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

-- | Specification of Pos.Chain.Block and Pos.Chain.Block.Pure.

module Test.Pos.Chain.Block.BlockSpec
       ( spec
       ) where

import           Universum

import           Serokell.Util (isVerSuccess)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, (===), (==>))

import           Pos.Binary.Class (Bi)
import           Pos.Chain.Block (BlockHeader (..), BlockSignature (..),
                     GenericBlockHeader (..), GenesisBlockchain,
                     GenesisBody (..), GenesisConsensusData (..),
                     GenesisExtraHeaderData (..), MainBlockchain,
                     MainBody (..), MainConsensusData (..),
                     MainExtraHeaderData (..), MainToSign (..), headerHash,
                     mkBodyProof, mkGenericHeader, mkGenesisHeader)
import qualified Pos.Chain.Block as Block
import           Pos.Core (EpochIndex (..), GenesisHash (..), HasConfiguration,
                     SlotId (..), difficultyL, genesisHash)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Core.Configuration (defaultCoreConfiguration,
                     withGenesisSpec)
import           Pos.Core.Delegation (HeavyDlgIndex (..), LightDlgIndices (..))
import           Pos.Crypto (ProtocolMagic (..), ProxySecretKey (pskIssuerPk),
                     SecretKey, SignTag (..), createPsk, proxySign, sign,
                     toPublic)

import           Test.Pos.Chain.Block.Arbitrary as BT
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

-- This tests are quite slow, hence max success is at most 20.
spec :: Spec
spec = withGenesisSpec 0 defaultCoreConfiguration id $ \_ ->
    describe "Block properties" $ modifyMaxSuccess (min 20) $ do
        describe "mkMainHeader" $ do
            prop mainHeaderFormationDesc mainHeaderFormation
        describe "mkGenesisHeader" $ do
            prop genesisHeaderFormationDesc genesisHeaderFormation
        describe "verifyHeader" $ do
            prop verifyHeaderDesc validateGoodMainHeader
            prop invalidProtocolMagicHeaderDesc
                 validateBadProtocolMagicMainHeader
        describe "verifyHeaders" $ modifyMaxSuccess (const 1) $ do
            prop verifyHeadersDesc validateGoodHeaderChain
            emptyHeaderChain (NewestFirst [])
  where
    mainHeaderFormationDesc
        = "Manually generating a main header block and using\
    \ mkMainHeader is the same"
    genesisHeaderFormationDesc
        = "Manually generating a genesis header block and using\
    \ mkGenesisHeader is the same"
    verifyHeaderDesc = "Successfully verifies a correct main block header"
    invalidProtocolMagicHeaderDesc =
        "Header with invalid protocol magic does not validate"
    verifyHeadersDesc =
        "Successfully verifies a correct chain of block headers"
    verifyEmptyHsDesc = "Successfully validates an empty header chain"
    emptyHeaderChain
        :: NewestFirst [] BlockHeader
        -> Spec
    emptyHeaderChain l =
        it verifyEmptyHsDesc $ isVerSuccess $ Block.verifyHeaders dummyProtocolMagic Nothing l

-- | Both of the following tests are boilerplate - they use `mkGenericHeader` to create
-- headers and then compare these with manually built headers.
--
-- This is to keep vigilant over changes in the behavior of `mkGenericHeader` because of
-- the ensuing failed tests.

genesisHeaderFormation
    :: HasConfiguration
    => Maybe BlockHeader
    -> EpochIndex
    -> GenesisBody
    -> Property
genesisHeaderFormation prevHeader epoch body = header === manualHeader
  where
    header = mkGenesisHeader
        dummyProtocolMagic
        (maybe (Left (GenesisHash genesisHash)) Right prevHeader)
        epoch
        body
    manualHeader = UnsafeGenericBlockHeader
        { _gbhProtocolMagic = dummyProtocolMagic
        , _gbhPrevBlock     = h
        , _gbhBodyProof     = proof
        , _gbhConsensus     = consensus h proof
        , _gbhExtra         = GenesisExtraHeaderData $ mkAttributes ()
        }
    h          = maybe genesisHash headerHash prevHeader
    proof      = mkBodyProof @GenesisBlockchain body
    difficulty = maybe 0 (view difficultyL) prevHeader
    consensus _ _ = GenesisConsensusData
        { _gcdEpoch      = epoch
        , _gcdDifficulty = difficulty
        }

mainHeaderFormation
    :: HasConfiguration
    => Maybe BlockHeader
    -> SlotId
    -> Either SecretKey (SecretKey, SecretKey, Bool)
    -> MainBody
    -> MainExtraHeaderData
    -> Property
mainHeaderFormation prevHeader slotId signer body extra =
    correctSigner signer ==> (header === manualHeader)
  where
    correctSigner (Left  _        ) = True
    correctSigner (Right (i, d, _)) = i /= d
    header = mkGenericHeader @MainBlockchain dummyProtocolMagic
                                                 prevHash
                                                 body
                                                 consensus
                                                 extra
    manualHeader =
        UnsafeGenericBlockHeader
        { _gbhProtocolMagic = dummyProtocolMagic
        , _gbhPrevBlock = prevHash
        , _gbhBodyProof = proof
        , _gbhConsensus = consensus proof
        , _gbhExtra = extra
        }
    prevHash = maybe genesisHash headerHash prevHeader
    proof = mkBodyProof @MainBlockchain body
    (sk, pSk) = either (, Nothing) mkProxySk signer
    mkProxySk (issuerSK, delegateSK, isSigEpoch) =
        let epoch = siEpoch slotId
            delegatePK = toPublic delegateSK
            curried :: Bi w => w -> ProxySecretKey w
            curried = createPsk dummyProtocolMagic issuerSK delegatePK
            proxy =
                if isSigEpoch
                    then Right $ curried $ HeavyDlgIndex epoch
                    else Left $ curried $ LightDlgIndices (epoch, epoch)
        in (delegateSK, Just $ proxy)
    difficulty = maybe 0 (succ . view difficultyL) prevHeader
    makeSignature toSign (Left psk) =
        BlockPSignatureLight $ proxySign dummyProtocolMagic SignMainBlockLight sk psk toSign
    makeSignature toSign (Right psk) =
        BlockPSignatureHeavy $ proxySign dummyProtocolMagic SignMainBlockHeavy sk psk toSign
    signature p =
        let toSign = MainToSign prevHash p slotId difficulty extra
        in maybe
               (BlockSignature (sign dummyProtocolMagic SignMainBlock sk toSign))
               (makeSignature toSign)
               pSk
    consensus p =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey =
              maybe (toPublic sk) (either pskIssuerPk pskIssuerPk) pSk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature p
        }

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

validateGoodMainHeader :: BT.HeaderAndParams -> Bool
validateGoodMainHeader (BT.getHAndP -> (params, header)) =
    isVerSuccess $ Block.verifyHeader dummyProtocolMagic params header

-- FIXME should sharpen this test to ensure that it fails with the expected
-- reason.
validateBadProtocolMagicMainHeader :: BT.HeaderAndParams -> Bool
validateBadProtocolMagicMainHeader (BT.getHAndP -> (params, header)) =
    let protocolMagic' = ProtocolMagic (getProtocolMagic dummyProtocolMagic + 1)
        header' = case header of
            BlockHeaderGenesis h -> BlockHeaderGenesis (h { _gbhProtocolMagic = protocolMagic' })
            BlockHeaderMain h    -> BlockHeaderMain    (h { _gbhProtocolMagic = protocolMagic' })
    in  not $ isVerSuccess $ Block.verifyHeader dummyProtocolMagic params header'

validateGoodHeaderChain :: BT.BlockHeaderList -> Bool
validateGoodHeaderChain (BT.BHL (l, _)) =
    isVerSuccess $ Block.verifyHeaders dummyProtocolMagic Nothing (NewestFirst l)
