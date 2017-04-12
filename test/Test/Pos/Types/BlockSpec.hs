{-# LANGUAGE RankNTypes #-}

-- | Specification of Pos.Types.Block.

module Test.Pos.Types.BlockSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe, it)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import           Pos.Binary            (Bi)
import           Pos.Block.Arbitrary   as T
import           Pos.Crypto            (ProxySecretKey (pskIssuerPk), SecretKey,
                                        SignTag (..), createProxySecretKey, proxySign,
                                        sign, toPublic)
import           Pos.Data.Attributes   (mkAttributes)
import           Pos.Ssc.Class         (Ssc (..), SscHelpersClass)
import           Pos.Ssc.GodTossing    (SscGodTossing)
import           Pos.Ssc.NistBeacon    (SscNistBeacon)
import qualified Pos.Types             as T
import           Pos.Util.Chrono       (NewestFirst (..))
import           Serokell.Util         (isVerSuccess)

import           Test.QuickCheck       (Property, (===))
spec :: Spec
spec = describe "Block properties" $ do
    describe "mkMainHeader" $ do
        prop mainHeaderFormationDesc (mainHeaderFormation @SscNistBeacon)
        prop mainHeaderFormationDesc (mainHeaderFormation @SscGodTossing)
    describe "mkGenesisHeader" $ do
        prop genesisHeaderFormationDesc (genesisHeaderFormation @SscNistBeacon)
        prop genesisHeaderFormationDesc (genesisHeaderFormation @SscGodTossing)
    describe "verifyHeader" $ do
        prop verifyHeaderDesc (validateGoodMainHeader @SscNistBeacon)
        prop verifyHeaderDesc (validateGoodMainHeader @SscGodTossing)
    describe "verifyHeaders" $ do
        prop verifyHeadersDesc (validateGoodHeaderChain @SscNistBeacon)
        prop verifyHeadersDesc (validateGoodHeaderChain @SscGodTossing)
        emptyHeaderChain (NewestFirst [] ::
                                 NewestFirst [] (T.BlockHeader SscNistBeacon))
        emptyHeaderChain (NewestFirst [] ::
                                 NewestFirst [] (T.BlockHeader SscGodTossing))
          where
    mainHeaderFormationDesc = "Manually generating a main header block and using\
    \ mkMainHeader is the same"
    genesisHeaderFormationDesc = "Manually generating a genesis header block and using\
    \ mkGenesisHeader is the same"
    verifyHeaderDesc = "Successfully verifies a correct main block header"
    verifyHeadersDesc = "Successfully verifies a correct chain of block headers"
    verifyEmptyHsDesc = "Successfully validates an empty header chain"
    emptyHeaderChain l =
        it verifyEmptyHsDesc $
            all isVerSuccess [T.verifyHeaders b l | b <- [False, True]]

-- | Both of the following tests are boilerplate - they use `mkGenericHeader` to create
-- headers and then compare these with manually built headers.
--
-- This is to keep vigilant over changes in the behavior of `mkGenericHeader` because of
-- the ensuing failed tests.

genesisHeaderFormation
    :: Ssc ssc
    => Maybe (T.BlockHeader ssc)
    -> T.EpochIndex
    -> T.Body (T.GenesisBlockchain ssc)
    -> Property
genesisHeaderFormation prevHeader epoch body =
    header === manualHeader
  where
    header = T.mkGenesisHeader prevHeader epoch body
    manualHeader =
        T.GenericBlockHeader
        { T._gbhPrevBlock = h
        , T._gbhBodyProof = proof
        , T._gbhConsensus = consensus h proof
        , T._gbhExtra = T.GenesisExtraHeaderData $ mkAttributes ()
        }
    h = maybe T.genesisHash T.headerHash prevHeader
    proof = T.mkBodyProof body
    difficulty = maybe 0 (view T.difficultyL) prevHeader
    consensus _ _ =
        T.GenesisConsensusData {T._gcdEpoch = epoch, T._gcdDifficulty = difficulty}

mainHeaderFormation
    :: SscHelpersClass ssc
    => Maybe (T.BlockHeader ssc)
    -> T.SlotId
    -> Either SecretKey (SecretKey, SecretKey, Bool)
    -> T.Body (T.MainBlockchain ssc)
    -> T.MainExtraHeaderData
    -> Property
mainHeaderFormation prevHeader slotId signer body extra =
    header === manualHeader
  where
    header = T.mkGenericHeader prevHeader body consensus extra
    manualHeader =
        T.GenericBlockHeader
        { T._gbhPrevBlock = h
        , T._gbhBodyProof = proof
        , T._gbhConsensus = consensus h proof
        , T._gbhExtra = extra
        }
    h = maybe T.genesisHash T.headerHash prevHeader
    proof = T.mkBodyProof body

    (sk, pSk) = either (, Nothing) mkProxySk signer
    mkProxySk (issuerSK, delegateSK, isSigEpoch) =
        let epoch = T.siEpoch slotId
            w = (epoch, epoch)
            delegatePK = toPublic delegateSK
            curried :: Bi w => w -> ProxySecretKey w
            curried = createProxySecretKey issuerSK delegatePK
            proxy = if isSigEpoch
                 then Right $ curried epoch
                 else Left $ curried w
        in (delegateSK, Just $ proxy)
    difficulty = maybe 0 (succ . view T.difficultyL) prevHeader
    makeSignature toSign (Left psk)  = T.BlockPSignatureEpoch $
        proxySign SignMainBlockLight sk psk toSign
    makeSignature toSign (Right psk) = T.BlockPSignatureSimple $
        proxySign SignMainBlockHeavy sk psk toSign
    signature prevHash p =
        let toSign = T.MainToSign prevHash p slotId difficulty extra
        in maybe (T.BlockSignature (sign SignMainBlock sk toSign))
                 (makeSignature toSign)
             pSk
    consensus prevHash p =
        T.MainConsensusData
        { T._mcdSlot = slotId
        , T._mcdLeaderKey = maybe (toPublic sk) (either pskIssuerPk pskIssuerPk) pSk
        , T._mcdDifficulty = difficulty
        , T._mcdSignature = signature prevHash p
        }

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

validateGoodMainHeader
    :: forall ssc . Ssc ssc
    => T.HeaderAndParams ssc -> Bool
validateGoodMainHeader (T.getHAndP -> (params, header)) =
    isVerSuccess $ T.verifyHeader params header

validateGoodHeaderChain
    :: forall ssc . Ssc ssc
    => Bool -> T.BlockHeaderList ssc -> Bool
validateGoodHeaderChain b (T.BHL (l, _)) =
    isVerSuccess $ T.verifyHeaders b (NewestFirst l)
