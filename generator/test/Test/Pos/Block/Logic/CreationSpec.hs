-- | Specification of 'Pos.Chain.Block.Creation' module.

module Test.Pos.Block.Logic.CreationSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Serokell.Data.Memory.Units (Byte, Gigabyte, convertUnit,
                     fromBytes)
import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Gen, Property, Testable, arbitrary, choose,
                     counterexample, elements, forAll, generate, listOf,
                     listOf1, oneof, property)

import           Pos.Binary.Class (biSize)
import           Pos.Chain.Block (BlockHeader, MainBlock)
import           Pos.Chain.Delegation (DlgPayload, ProxySKBlockInfo)
import           Pos.Chain.Genesis as Genesis (Config (..), GenesisData (..))
import           Pos.Chain.Ssc (SscPayload (..), defaultSscPayload,
                     mkVssCertificatesMapLossy)
import           Pos.Chain.Txp (TxAux)
import           Pos.Chain.Update (BlockVersionData (..),
                     HasUpdateConfiguration, UpdatePayload (..),
                     updateConfiguration)
import qualified Pos.Communication ()
import           Pos.Core (SlotId (..), kEpochSlots, localSlotIndexMinBound,
                     pcBlkSecurityParam, unsafeMkLocalSlotIndex)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..),
                     SecretKey)
import           Pos.DB.Block (RawPayload (..), createMainBlockPure)

import           Test.Pos.Chain.Block.Arbitrary ()
import           Test.Pos.Chain.Delegation.Arbitrary (genDlgPayload)
import           Test.Pos.Chain.Ssc.Arbitrary (commitmentMapEpochGen,
                     vssCertificateEpochGen)
import           Test.Pos.Chain.Txp.Arbitrary (GoodTx, goodTxToTxAux)
import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Util.QuickCheck (SmallGenerator (..), makeSmall)

spec :: Spec
spec = do
    runWithMagic RequiresNoMagic
    runWithMagic RequiresMagic

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $ \genesisConfig _ _ ->
  describe "Block.Logic.Creation" $ do

    -- Sampling the minimum empty block size
    (sk0,prevHeader0) <- runIO $ generate arbitrary

    -- We multiply by 1.5 because there are different possible
    -- combinations of empty constructors and there's no out-of-box
    -- way to get maximum of them. Some settings produce 390b empty
    -- block, some -- 431b.
    let emptyBSize0 :: Byte
        emptyBSize0 = biSize (noSscBlock genesisConfig infLimit prevHeader0 [] def def sk0) -- in bytes
        emptyBSize :: Integral n => n
        emptyBSize = round $ (1.5 * fromIntegral emptyBSize0 :: Double)

    describe "createMainBlockPure" $ modifyMaxSuccess (const 1000) $ do
        prop "empty block size is sane" $ emptyBlk genesisConfig $ \blk0 -> leftToCounter blk0 $ \blk ->
            let s = biSize blk
            in counterexample ("Real block size: " <> show s <>
                               "\n\nBlock: " <> show blk) $
                 -- Various hashes and signatures in the block take 416
                 -- bytes; this is *completely* independent of encoding used.
                 -- Empirically, empty blocks don't get bigger than 550
                 -- bytes.
                 s <= 550 && s <= bvdMaxBlockSize (gdBlockVersionData $ configGenesisData genesisConfig)
        prop "doesn't create blocks bigger than the limit" $
            forAll (choose (emptyBSize, emptyBSize * 10)) $ \(fromBytes -> limit) ->
            forAll arbitrary $ \(prevHeader, sk, updatePayload) ->
            forAll (validSscPayloadGen genesisConfig) $ \(sscPayload, slotId) ->
            forAll (genDlgPayload pm (siEpoch slotId)) $ \dlgPayload ->
            forAll (makeSmall $ listOf1 genTxAux) $ \txs ->
            let blk = producePureBlock genesisConfig limit prevHeader txs Nothing slotId
                                       dlgPayload sscPayload updatePayload sk
            in leftToCounter blk $ \b ->
                let s = biSize b
                in counterexample ("Real block size: " <> show s) $
                   s <= fromIntegral limit
        prop "removes transactions when necessary" $
            forAll arbitrary $ \(prevHeader, sk) ->
            forAll (makeSmall $ listOf1 genTxAux) $ \txs ->
            forAll (elements [0,0.5,0.9]) $ \(delta :: Double) ->
            let blk0 = noSscBlock genesisConfig infLimit prevHeader [] def def sk
                blk1 = noSscBlock genesisConfig infLimit prevHeader txs def def sk
            in leftToCounter ((,) <$> blk0 <*> blk1) $ \(b0, b1) ->
                let s = biSize b0 +
                        round ((fromIntegral $ biSize b1 - biSize b0) * delta)
                    blk2 = noSscBlock genesisConfig s prevHeader txs def def sk
                in counterexample ("Tested with block size limit: " <> show s) $
                   leftToCounter blk2 (const True)
        prop "strips ssc data when necessary" $
            forAll arbitrary $ \(prevHeader, sk) ->
            forAll (validSscPayloadGen genesisConfig) $ \(sscPayload, slotId) ->
            forAll (elements [0,0.5,0.9]) $ \(delta :: Double) ->
            let blk0 = producePureBlock genesisConfig infLimit prevHeader [] Nothing
                                        slotId def (defSscPld genesisConfig slotId) def sk
                withPayload lim =
                    producePureBlock genesisConfig lim prevHeader [] Nothing slotId def sscPayload def sk
                blk1 = withPayload infLimit
            in leftToCounter ((,) <$> blk0 <*> blk1) $ \(b0,b1) ->
                let s = biSize b0 +
                        round ((fromIntegral $ biSize b1 - biSize b0) * delta)
                    blk2 = withPayload s
                in counterexample ("Tested with block size limit: " <> show s) $
                   leftToCounter blk2 (const True)
  where
    defSscPld :: Genesis.Config -> SlotId -> SscPayload
    defSscPld genesisConfig sId = do
        let k = pcBlkSecurityParam $ configProtocolConstants genesisConfig
        defaultSscPayload k $ siSlot sId

    infLimit = convertUnit @Gigabyte @Byte 1

    leftToCounter :: (ToString s, Testable p) => Either s a -> (a -> p) -> Property
    leftToCounter x c = either (\t -> counterexample (toString t) False) (property . c) x

    emptyBlk
        :: (HasUpdateConfiguration, Testable p)
        => Genesis.Config
        -> (Either Text MainBlock -> p)
        -> Property
    emptyBlk genesisConfig foo =
        forAll arbitrary $ \(prevHeader, sk, slotId) ->
        foo $ producePureBlock genesisConfig infLimit prevHeader [] Nothing slotId def (defSscPld genesisConfig slotId) def sk

    genTxAux :: Gen TxAux
    genTxAux =
        goodTxToTxAux . getSmallGenerator <$> (arbitrary :: Gen (SmallGenerator GoodTx))

    noSscBlock
        :: HasUpdateConfiguration
        => Genesis.Config
        -> Byte
        -> BlockHeader
        -> [TxAux]
        -> DlgPayload
        -> UpdatePayload
        -> SecretKey
        -> Either Text MainBlock
    noSscBlock genesisConfig limit prevHeader txs proxyCerts updatePayload sk =
        let k          = pcBlkSecurityParam $ configProtocolConstants genesisConfig
            epochSlots = kEpochSlots k
            neutralSId = SlotId 0 (unsafeMkLocalSlotIndex epochSlots $ fromIntegral $ k * 2)
        in producePureBlock
             genesisConfig limit prevHeader txs Nothing neutralSId proxyCerts (defSscPld genesisConfig neutralSId) updatePayload sk

    producePureBlock
        :: HasUpdateConfiguration
        => Genesis.Config
        -> Byte
        -> BlockHeader
        -> [TxAux]
        -> ProxySKBlockInfo
        -> SlotId
        -> DlgPayload
        -> SscPayload
        -> UpdatePayload
        -> SecretKey
        -> Either Text MainBlock
    producePureBlock genesisConfig limit prev txs psk slot dlgPay sscPay usPay sk =
        flip runReaderT updateConfiguration $
        createMainBlockPure genesisConfig limit prev psk slot sk $
        RawPayload txs sscPay dlgPay usPay

validSscPayloadGen :: Genesis.Config -> Gen (SscPayload, SlotId)
validSscPayloadGen genesisConfig = do
    let pm                = configProtocolMagic genesisConfig
        protocolConstants = configProtocolConstants genesisConfig
        k                 = pcBlkSecurityParam protocolConstants
        epochSlots        = kEpochSlots k
    vssCerts <- makeSmall $ fmap mkVssCertificatesMapLossy $ listOf $
        vssCertificateEpochGen pm protocolConstants 0
    let mkSlot i = SlotId 0 (unsafeMkLocalSlotIndex epochSlots (fromIntegral i))
    oneof [ do commMap <- makeSmall $ commitmentMapEpochGen pm 0
               pure (CommitmentsPayload commMap vssCerts , SlotId 0 localSlotIndexMinBound)
          , do openingsMap <- makeSmall arbitrary
               pure (OpeningsPayload openingsMap vssCerts, mkSlot (4 * k + 1))
          , do sharesMap <- makeSmall arbitrary
               pure (SharesPayload sharesMap vssCerts, mkSlot (8 * k))
          , pure (CertificatesPayload vssCerts, mkSlot (7 * k))
          ]
