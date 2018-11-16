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
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Gen, Property, (===), (==>))

import           Pos.Chain.Block (BlockHeader (..), BlockSignature (..),
                     GenesisBody (..), GenesisConsensusData (..),
                     GenesisExtraHeaderData (..), MainBody (..),
                     MainConsensusData (..), MainExtraHeaderData (..),
                     MainToSign (..), gbhProtocolMagicId, headerHash,
                     mkGenericBlockHeaderUnsafe, mkGenesisHeader,
                     mkGenesisProof, mkMainHeader, mkMainProof)
import qualified Pos.Chain.Block as Block
import           Pos.Chain.Delegation (HeavyDlgIndex (..))
import           Pos.Chain.Genesis (GenesisHash (..))
import           Pos.Chain.Update (ConsensusEra)
import           Pos.Core (EpochIndex (..), SlotId (..), difficultyL)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     ProxySecretKey (pskIssuerPk), SecretKey, SignTag (..),
                     createPsk, getProtocolMagic, proxySign, sign, toPublic)

import           Test.Pos.Chain.Block.Arbitrary as BT
import           Test.Pos.Chain.Genesis.Dummy (dummyGenesisHash)

-- This tests are quite slow, hence max success is at most 20.
spec :: Spec
spec = describe "Block properties" $ modifyMaxSuccess (min 20) $ do
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
            prop verifyEmptyHsDesc (emptyHeaderChain (NewestFirst []))
  where
    mainHeaderFormationDesc =
        "Manually generating a main header block and using mkMainHeader is the same"
    genesisHeaderFormationDesc =
        "Manually generating a genesis header block and using mkGenesisHeader is the same"
    verifyHeaderDesc = "Successfully verifies a correct main block header"
    invalidProtocolMagicHeaderDesc =
        "Header with invalid protocol magic does not validate"
    verifyHeadersDesc =
        "Successfully verifies a correct chain of block headers"
    verifyEmptyHsDesc = "Successfully validates an empty header chain"
    emptyHeaderChain
        :: NewestFirst [] BlockHeader
        -> ProtocolMagic
        -> ConsensusEra
        -> Bool
    emptyHeaderChain l pm era =
        isVerSuccess $ Block.verifyHeaders pm era Nothing l

-- | Both of the following tests are boilerplate - they use `mkGenericHeader` to create
-- headers and then compare these with manually built headers.
--
-- This is to keep vigilant over changes in the behavior of `mkGenericHeader` because of
-- the ensuing failed tests.

genesisHeaderFormation
    :: ProtocolMagic
    -> Maybe BlockHeader
    -> EpochIndex
    -> GenesisBody
    -> Property
genesisHeaderFormation pm prevHeader epoch body = header === manualHeader
  where
    header = mkGenesisHeader
        pm
        (maybe (Left dummyGenesisHash) Right prevHeader)
        epoch
        body
    manualHeader = mkGenericBlockHeaderUnsafe
        pm
        h
        proof
        (consensus h proof)
        (GenesisExtraHeaderData $ mkAttributes ())
    h          = maybe (getGenesisHash dummyGenesisHash) headerHash prevHeader
    proof      = mkGenesisProof body
    difficulty = maybe 0 (view difficultyL) prevHeader
    consensus _ _ = GenesisConsensusData
        { _gcdEpoch      = epoch
        , _gcdDifficulty = difficulty
        }

mainHeaderFormation
    :: ProtocolMagic
    -> Maybe BlockHeader
    -> SlotId
    -> Either SecretKey (SecretKey, SecretKey)
    -> MainBody
    -> MainExtraHeaderData
    -> Property
mainHeaderFormation pm prevHeader slotId signer body extra =
    correctSigner signer ==> (header === manualHeader)
  where
    correctSigner (Left  _        ) = True
    correctSigner (Right (i, d))    = i /= d
    header = mkMainHeader pm prevHeader' slotId sk pske body extra
    manualHeader =
        mkGenericBlockHeaderUnsafe
            pm
            prevHash
            proof
            (consensus proof)
            extra
    prevHash = maybe (getGenesisHash dummyGenesisHash) headerHash prevHeader
    prevHeader' = maybe (Left dummyGenesisHash) Right prevHeader
    proof = mkMainProof body
    (sk, pSk) = either (, Nothing) mkProxySk signer
    mkProxySk (issuerSK, delegateSK) =
        let epoch = siEpoch slotId
            delegatePK = toPublic delegateSK
            proxy = createPsk pm issuerSK delegatePK (HeavyDlgIndex epoch)
        in (delegateSK, Just proxy)
    pske = pSk >>= \proxy -> Just (proxy, pskIssuerPk proxy)
    difficulty = maybe 0 (succ . view difficultyL) prevHeader
    makeSignature toSign psk =
        BlockPSignatureHeavy $ proxySign pm SignMainBlockHeavy sk psk toSign
    signature p =
        let toSign = MainToSign prevHash p slotId difficulty extra
        in maybe
               (BlockSignature (sign pm SignMainBlock sk toSign))
               (makeSignature toSign)
               pSk
    consensus p =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey =
              maybe (toPublic sk) pskIssuerPk pSk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature p
        }

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

validateGoodMainHeader :: ProtocolMagic -> Gen Bool
validateGoodMainHeader pm = do
    (BT.getHAndP -> (params, header)) <- BT.genHeaderAndParams pm
    pure $ isVerSuccess $ Block.verifyHeader pm params header

-- FIXME should sharpen this test to ensure that it fails with the expected
-- reason.
validateBadProtocolMagicMainHeader :: ProtocolMagic -> Gen Bool
validateBadProtocolMagicMainHeader pm = do
    (BT.getHAndP -> (params, header)) <- BT.genHeaderAndParams pm
    let protocolMagicId' = ProtocolMagicId (getProtocolMagic pm + 1)
        header' = case header of
            BlockHeaderGenesis h -> BlockHeaderGenesis (h & gbhProtocolMagicId .~ protocolMagicId')
            BlockHeaderMain h    -> BlockHeaderMain    (h & gbhProtocolMagicId .~ protocolMagicId')
    pure $ not $ isVerSuccess $ Block.verifyHeader pm params header'

validateGoodHeaderChain :: ProtocolMagic -> ConsensusEra -> Gen Bool
validateGoodHeaderChain pm era = do
    BT.BHL (l, _) <- BT.genStubbedBHL pm
    pure $ isVerSuccess $ Block.verifyHeaders pm era Nothing (NewestFirst l)
