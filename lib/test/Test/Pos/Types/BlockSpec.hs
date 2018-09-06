{-# LANGUAGE RankNTypes #-}

-- | Specification of Pos.Core.Block and Pos.Block.Pure.

module Test.Pos.Types.BlockSpec
       ( spec
       ) where

import           Universum

import           Serokell.Util (isVerSuccess)
import           Test.Hspec (Spec, describe, it, runIO)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, arbitrary, generate, (===), (==>))

import           Pos.Binary (Bi)
import qualified Pos.Block.Logic.Integrity as T
import           Pos.Core (GenesisHash (..), HasConfiguration, genesisHash)
import qualified Pos.Core as T
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..), ProxySecretKey (pskIssuerPk),
                             RequiresNetworkMagic (..), SecretKey, SignTag (..), createPsk,
                             getProtocolMagic, proxySign, sign, toPublic)
import           Pos.Data.Attributes (mkAttributes)

import           Test.Pos.Block.Arbitrary as T
import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic, dummyProtocolMagicId)

-- This tests are quite slow, hence max success is at most 20.
spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $
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
        :: NewestFirst [] T.BlockHeader
        -> Spec
    emptyHeaderChain l =
        it verifyEmptyHsDesc $ isVerSuccess $ T.verifyHeaders dummyProtocolMagic Nothing l

-- | Both of the following tests are boilerplate - they use `mkGenericHeader` to create
-- headers and then compare these with manually built headers.
--
-- This is to keep vigilant over changes in the behavior of `mkGenericHeader` because of
-- the ensuing failed tests.

genesisHeaderFormation
    :: HasConfiguration
    => Maybe T.BlockHeader
    -> T.EpochIndex
    -> T.GenesisBody
    -> Property
genesisHeaderFormation prevHeader epoch body = header === manualHeader
  where
    header = T.mkGenesisHeader
        dummyProtocolMagic
        (maybe (Left (GenesisHash genesisHash)) Right prevHeader)
        epoch
        body
    manualHeader = T.UnsafeGenericBlockHeader
        { T._gbhProtocolMagicId = dummyProtocolMagicId
        , T._gbhPrevBlock       = h
        , T._gbhBodyProof       = proof
        , T._gbhConsensus       = consensus h proof
        , T._gbhExtra           = T.GenesisExtraHeaderData $ mkAttributes ()
        }
    h          = maybe genesisHash T.headerHash prevHeader
    proof      = T.mkBodyProof @T.GenesisBlockchain body
    difficulty = maybe 0 (view T.difficultyL) prevHeader
    consensus _ _ = T.GenesisConsensusData
        { T._gcdEpoch      = epoch
        , T._gcdDifficulty = difficulty
        }

mainHeaderFormation
    :: HasConfiguration
    => Maybe T.BlockHeader
    -> T.SlotId
    -> Either SecretKey (SecretKey, SecretKey, Bool)
    -> T.MainBody
    -> T.MainExtraHeaderData
    -> Property
mainHeaderFormation prevHeader slotId signer body extra =
    correctSigner signer ==> (header === manualHeader)
  where
    correctSigner (Left  _        ) = True
    correctSigner (Right (i, d, _)) = i /= d
    header = T.mkGenericHeader @T.MainBlockchain dummyProtocolMagic
                                                 prevHash
                                                 body
                                                 consensus
                                                 extra
    manualHeader =
        T.UnsafeGenericBlockHeader
        { T._gbhProtocolMagicId = dummyProtocolMagicId
        , T._gbhPrevBlock = prevHash
        , T._gbhBodyProof = proof
        , T._gbhConsensus = consensus proof
        , T._gbhExtra = extra
        }
    prevHash = maybe genesisHash T.headerHash prevHeader
    proof = T.mkBodyProof @T.MainBlockchain body
    (sk, pSk) = either (, Nothing) mkProxySk signer
    mkProxySk (issuerSK, delegateSK, isSigEpoch) =
        let epoch = T.siEpoch slotId
            delegatePK = toPublic delegateSK
            curried :: Bi w => w -> ProxySecretKey w
            curried = createPsk dummyProtocolMagic issuerSK delegatePK
            proxy =
                if isSigEpoch
                    then Right $ curried $ T.HeavyDlgIndex epoch
                    else Left $ curried $ T.LightDlgIndices (epoch, epoch)
        in (delegateSK, Just $ proxy)
    difficulty = maybe 0 (succ . view T.difficultyL) prevHeader
    makeSignature toSign (Left psk) =
        T.BlockPSignatureLight $ proxySign dummyProtocolMagic SignMainBlockLight sk psk toSign
    makeSignature toSign (Right psk) =
        T.BlockPSignatureHeavy $ proxySign dummyProtocolMagic SignMainBlockHeavy sk psk toSign
    signature p =
        let toSign = T.MainToSign prevHash p slotId difficulty extra
        in maybe
               (T.BlockSignature (sign dummyProtocolMagic SignMainBlock sk toSign))
               (makeSignature toSign)
               pSk
    consensus p =
        T.MainConsensusData
        { T._mcdSlot = slotId
        , T._mcdLeaderKey =
              maybe (toPublic sk) (either pskIssuerPk pskIssuerPk) pSk
        , T._mcdDifficulty = difficulty
        , T._mcdSignature = signature p
        }

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

validateGoodMainHeader :: T.HeaderAndParams -> Bool
validateGoodMainHeader (T.getHAndP -> (params, header)) =
    isVerSuccess $ T.verifyHeader dummyProtocolMagic params header

-- FIXME should sharpen this test to ensure that it fails with the expected
-- reason.
validateBadProtocolMagicMainHeader :: T.HeaderAndParams -> Bool
validateBadProtocolMagicMainHeader (T.getHAndP -> (params, header)) =
    let protocolMagic' = ProtocolMagicId (getProtocolMagic dummyProtocolMagic + 1)
        header' = case header of
            T.BlockHeaderGenesis h -> T.BlockHeaderGenesis (h { T._gbhProtocolMagicId = protocolMagic' })
            T.BlockHeaderMain h    -> T.BlockHeaderMain    (h { T._gbhProtocolMagicId = protocolMagic' })
    in  not $ isVerSuccess $ T.verifyHeader dummyProtocolMagic params header'

validateGoodHeaderChain :: T.BlockHeaderList -> Bool
validateGoodHeaderChain (T.BHL (l, _)) =
    isVerSuccess $ T.verifyHeaders dummyProtocolMagic Nothing (NewestFirst l)
