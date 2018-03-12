{-# LANGUAGE RankNTypes #-}

-- | Specification of Pos.Core.Block and Pos.Block.Pure.

module Test.Pos.Types.BlockSpec
       ( spec
       ) where

import           Universum

import           Serokell.Util (isVerSuccess)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, (===), (==>))

import           Pos.Arbitrary.Block as T
import           Pos.Binary (Bi)
import qualified Pos.Block.Base as T
import qualified Pos.Block.Logic.Integrity as T
import           Pos.Core (HasConfiguration, genesisHash)
import qualified Pos.Core as T
import           Pos.Crypto (ProxySecretKey (pskIssuerPk), SecretKey, SignTag (..), createPsk,
                             proxySign, sign, toPublic)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Util.Chrono (NewestFirst (..))

import           Test.Pos.Configuration (withDefConfiguration)

-- This tests are quite slow, hence max success is at most 20.
spec :: Spec
spec = withDefConfiguration $ describe "Block properties" $ modifyMaxSuccess (min 20) $ do
    describe "mkMainHeader" $ do
        prop mainHeaderFormationDesc mainHeaderFormation
    describe "mkGenesisHeader" $ do
        prop genesisHeaderFormationDesc genesisHeaderFormation
    describe "verifyHeader" $ do
        prop verifyHeaderDesc validateGoodMainHeader
    describe "verifyHeaders" $ modifyMaxSuccess (const 1) $ do
        prop verifyHeadersDesc validateGoodHeaderChain
        emptyHeaderChain (NewestFirst [])
          where
    mainHeaderFormationDesc = "Manually generating a main header block and using\
    \ mkMainHeader is the same"
    genesisHeaderFormationDesc = "Manually generating a genesis header block and using\
    \ mkGenesisHeader is the same"
    verifyHeaderDesc = "Successfully verifies a correct main block header"
    verifyHeadersDesc = "Successfully verifies a correct chain of block headers"
    verifyEmptyHsDesc = "Successfully validates an empty header chain"
    emptyHeaderChain ::
           HasConfiguration
        => NewestFirst [] T.BlockHeader
        -> Spec
    emptyHeaderChain l =
        it verifyEmptyHsDesc $ isVerSuccess $ T.verifyHeaders Nothing l

-- | Both of the following tests are boilerplate - they use `mkGenericHeader` to create
-- headers and then compare these with manually built headers.
--
-- This is to keep vigilant over changes in the behavior of `mkGenericHeader` because of
-- the ensuing failed tests.

genesisHeaderFormation
    :: HasConfiguration
    => Maybe T.BlockHeader
    -> T.EpochIndex
    -> T.Body T.GenesisBlockchain
    -> Property
genesisHeaderFormation prevHeader epoch body =
    header === manualHeader
  where
    header = T.mkGenesisHeader prevHeader epoch body
    manualHeader =
        T.UncheckedGenericBlockHeader
        { T._gbhPrevBlock = h
        , T._gbhBodyProof = proof
        , T._gbhConsensus = consensus h proof
        , T._gbhExtra = T.GenesisExtraHeaderData $ mkAttributes ()
        }
    h = maybe genesisHash T.headerHash prevHeader
    proof = T.mkBodyProof body
    difficulty = maybe 0 (view T.difficultyL) prevHeader
    consensus _ _ =
        T.GenesisConsensusData {T._gcdEpoch = epoch, T._gcdDifficulty = difficulty}

mainHeaderFormation
    :: HasConfiguration
    => Maybe T.BlockHeader
    -> T.SlotId
    -> Either SecretKey (SecretKey, SecretKey, Bool)
    -> T.Body T.MainBlockchain
    -> T.MainExtraHeaderData
    -> Property
mainHeaderFormation prevHeader slotId signer body extra =
    correctSigner signer ==> (header === manualHeader)
  where
    correctSigner (Left _)        = True
    correctSigner (Right (i,d,_)) = i /= d
    header = T.mkGenericHeader prevHeader body consensus extra
    manualHeader =
        T.UncheckedGenericBlockHeader
        { T._gbhPrevBlock = h
        , T._gbhBodyProof = proof
        , T._gbhConsensus = consensus h proof
        , T._gbhExtra = extra
        }
    h = maybe genesisHash T.headerHash prevHeader
    proof = T.mkBodyProof body
    (sk, pSk) = either (, Nothing) mkProxySk signer
    mkProxySk (issuerSK, delegateSK, isSigEpoch) =
        let epoch = T.siEpoch slotId
            delegatePK = toPublic delegateSK
            curried :: Bi w => w -> ProxySecretKey w
            curried = createPsk issuerSK delegatePK
            proxy =
                if isSigEpoch
                    then Right $ curried $ T.HeavyDlgIndex epoch
                    else Left $ curried $ T.LightDlgIndices (epoch, epoch)
        in (delegateSK, Just $ proxy)
    difficulty = maybe 0 (succ . view T.difficultyL) prevHeader
    makeSignature toSign (Left psk) =
        T.BlockPSignatureLight $ proxySign SignMainBlockLight sk psk toSign
    makeSignature toSign (Right psk) =
        T.BlockPSignatureHeavy $ proxySign SignMainBlockHeavy sk psk toSign
    signature prevHash p =
        let toSign = T.MainToSign prevHash p slotId difficulty extra
        in maybe
               (T.BlockSignature (sign SignMainBlock sk toSign))
               (makeSignature toSign)
               pSk
    consensus prevHash p =
        T.MainConsensusData
        { T._mcdSlot = slotId
        , T._mcdLeaderKey =
              maybe (toPublic sk) (either pskIssuerPk pskIssuerPk) pSk
        , T._mcdDifficulty = difficulty
        , T._mcdSignature = signature prevHash p
        }

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

validateGoodMainHeader
    :: HasConfiguration
    => T.HeaderAndParams -> Bool
validateGoodMainHeader (T.getHAndP -> (params, header)) =
    isVerSuccess $ T.verifyHeader params header

validateGoodHeaderChain
    :: HasConfiguration
    => T.BlockHeaderList -> Bool
validateGoodHeaderChain (T.BHL (l, _)) =
    isVerSuccess $ T.verifyHeaders Nothing (NewestFirst l)
