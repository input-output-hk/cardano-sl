{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Specification of Pos.Types.Block.

module Test.Pos.Types.BlockSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe, it)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import           Control.Lens          (view, (^.))
import           Data.List.NonEmpty    (fromList)
import           Pos.Types.Address     (addressHash)
import           Pos.Binary            (Bi)
import           Pos.Block.Arbitrary   as T
import           Pos.Constants         (epochSlots)
import           Pos.Crypto            (ProxySecretKey (pskIssuerPk), SecretKey,
                                        createProxySecretKey, hash, proxySign, sign,
                                        toPublic)
import           Pos.Ssc.Class.Types   (Ssc (..))
import           Pos.Ssc.GodTossing    (SscGodTossing)
import           Pos.Ssc.NistBeacon    (SscNistBeacon)
import qualified Pos.Types             as T
import           Serokell.Util         (isVerSuccess)
import           System.Random         (mkStdGen, randomR)

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
        emptyHeaderChain ([] :: [T.BlockHeader SscNistBeacon])
        emptyHeaderChain ([] :: [T.BlockHeader SscGodTossing])
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
            and
                [isVerSuccess $
                    T.verifyHeaders b l
                        | b <- [False, True]] == True

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
    header = (T.mkGenesisHeader prevHeader epoch body)
    manualHeader =
        T.GenericBlockHeader
        { T._gbhPrevBlock = h
        , T._gbhBodyProof = proof
        , T._gbhConsensus = consensus h proof
        , T._gbhExtra = ()
        }
    h = maybe T.genesisHash hash prevHeader
    proof = T.mkBodyProof body
    difficulty = maybe 0 (view T.difficultyL) prevHeader
    consensus _ _ =
        T.GenesisConsensusData {T._gcdEpoch = epoch, T._gcdDifficulty = difficulty}

mainHeaderFormation
    :: Ssc ssc
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
    h = maybe T.genesisHash hash prevHeader
    proof = T.mkBodyProof body

    (sk, pSk) = either (, Nothing) mkProxySk signer
    mkProxySk (issuerSK, delegateSK, isSigEpoch) =
        let epoch = T.siEpoch slotId
            w = (epoch, epoch)
            delegatePK = toPublic delegateSK
            curried :: Bi w => w -> ProxySecretKey w
            curried = createProxySecretKey issuerSK delegatePK
            proxy = if isSigEpoch
                 then Right  $ curried ()
                 else Left $ curried w
        in (delegateSK, Just $ proxy)
    difficulty = maybe 0 (succ . view T.difficultyL) prevHeader
    makeSignature toSign (Left psk)  = T.BlockPSignatureEpoch $ proxySign sk psk toSign
    makeSignature toSign (Right psk) = T.BlockPSignatureSimple $ proxySign sk psk toSign
    signature prevHash p =
        let toSign = (prevHash, p, slotId, difficulty)
        in maybe (T.BlockSignature $ sign sk toSign) (makeSignature toSign) pSk
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
    => Bool -> Int -> T.BlockHeaderList ssc -> Bool
validateGoodMainHeader
    consensus
    seed
    (first reverse . T.getHeaderList -> (blocks, leaders)) =
    let atMost3Blocks = take 3 blocks
        (prev, block, next) =
            case atMost3Blocks of
                [b] -> (Nothing, b, Nothing)
                [b1, b2] -> (Just b1, b2, Nothing)
                (b1 : b2 : b3 : _) -> (Just b1, b2, Just b3)
                _ -> panic "[BlockSpec] the blockchain doesn't have enough blocks"
        thisEpochStartIndex = fromIntegral $ epochSlots * (block ^. T.epochIndexL)
        thisBlocksEpoch = drop thisEpochStartIndex leaders
        randomSlotBeforeThisBlock =
            case block of
                Left _  -> Nothing
                Right h ->
                    let (T.SlotId e s) = view T.headerSlot h
                        betweenZeroAnd n = fst . randomR (0, n) . mkStdGen $ seed
                        rndSlot = T.SlotId (betweenZeroAnd e)
                                           (betweenZeroAnd s)
                    in Just rndSlot
        params = T.VerifyHeaderParams
            { T.vhpVerifyConsensus = consensus
            , T.vhpPrevHeader = prev
            , T.vhpNextHeader = next
            , T.vhpCurrentSlot = randomSlotBeforeThisBlock
            , T.vhpLeaders = Just $ fromList $ map addressHash thisBlocksEpoch
            }
    in isVerSuccess $ T.verifyHeader params block

validateGoodHeaderChain
    :: forall ssc . Ssc ssc
    => Bool -> T.BlockHeaderList ssc -> Bool
validateGoodHeaderChain b (fst . T.getHeaderList -> l) =
    isVerSuccess $ T.verifyHeaders b l
