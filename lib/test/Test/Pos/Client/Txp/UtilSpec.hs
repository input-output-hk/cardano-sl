{-# LANGUAGE TypeFamilies #-}

-- | Specification of Pos.Client.Txp.Util

module Test.Pos.Client.Txp.UtilSpec
       ( spec
       ) where

import           Universum

import qualified Data.HashMap.Strict      as HM
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Formatting               (build, hex, left, sformat, shown, (%), (%.))
import           Test.Hspec               (Spec, describe)
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck          (Discard (..), Gen, arbitrary, choose)
import           Test.QuickCheck.Monadic  (forAllM, stop)

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Util      (TxError (..), TxOutputs, TxWithSpendings,
                                           createMTx, createRedemptionTx,
                                           isNotEnoughMoneyTxError)
import           Pos.Core                 (BlockVersionData (..), Coeff (..),
                                           TxFeePolicy (..), TxSizeLinear (..),
                                           makePubKeyAddressBoot, makeRedeemAddress,
                                           unsafeIntegerToCoin)
import           Pos.Crypto               (RedeemSecretKey, SafeSigner, SecretKey,
                                           decodeHash, fakeSigner, redeemToPublic,
                                           toPublic)
import           Pos.DB                   (gsAdoptedBVData)
import           Pos.Txp                  (Tx (..), TxAux (..), TxId, TxIn (..),
                                           TxOut (..), TxOutAux (..), Utxo)
import           Pos.Types                (Address)
import           Pos.Util.Arbitrary       (nonrepeating)
import           Pos.Util.Util            (leftToPanic)
import           Test.Pos.Util            (stopProperty, withDefConfigurations)

import           Test.Pos.Client.Txp.Mode (HasTxpConfigurations, TxpTestMode,
                                           TxpTestProperty, withBVData)

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

spec :: Spec
spec = withDefConfigurations $
    describe "Client.Txp.Util" $ do
        describe "createMTx" $ createMTxSpec

createMTxSpec :: HasTxpConfigurations => Spec
createMTxSpec = do
    prop createMTxWorksWhenWeAreRichDesc createMTxWorksWhenWeAreRichSpec
    prop stabilizationDoesNotFailDesc stabilizationDoesNotFailSpec
    prop feeIsNonzeroDesc feeIsNonzeroSpec
    prop manyUtxoTo1Desc manyUtxoTo1Spec
    prop manyAddressesTo1Desc manyAddressesTo1Spec
    prop manyAddressesToManyDesc manyAddressesToManySpec
    prop redemptionDesc redemptionSpec
    prop txWithRedeemOutputFailsDesc txWithRedeemOutputFailsSpec
    prop feeForManyAddressesDesc feeForManyAddressesSpec
  where
    createMTxWorksWhenWeAreRichDesc =
        "Transaction is created successfully when we have 1 input with 1M coins " <>
        "and 1 output with 1 coin"
    stabilizationDoesNotFailDesc =
        "FailedToStabilize is not thrown " <>
        "when there is 1 input with 200k coins and 1 output with 1 coin"
    feeIsNonzeroDesc =
        "An attempt to create a tx for 1 coin when we have 100k coins fails " <>
        "because of the fee"
    manyUtxoTo1Desc =
        "Transaction is created successfully when we have 10 items in Utxo " <>
        "for a single address with 100k coins each"
    manyAddressesTo1Desc =
        "Transaction is created successfully when we have 10 items in Utxo " <>
        "for 10 different addresses with 100k coins each and 1 output with 1 coin"
    manyAddressesToManyDesc =
        "Transaction is created successfully when we have 10 items in Utxo " <>
        "for 10 different addresses with 100k coins each and 10 outputs with 1 coin each"
    redemptionDesc =
        "Redemption transaction is created successfully"
    txWithRedeemOutputFailsDesc =
        "An attempt to create a tx with a redeem address as an output fails"
    feeForManyAddressesDesc =
        "Fee evaluation succeedes when many addresses are used"

createMTxWorksWhenWeAreRichSpec :: HasTxpConfigurations => TxpTestProperty ()
createMTxWorksWhenWeAreRichSpec =
    forAllM gen $ \(CreateMTxParams {..}) -> do
        txOrError <- createMTx cmpUtxo (getSignerFromList cmpSigners) cmpOutputs cmpAddrData
        case txOrError of
            Left err -> stopProperty $ sformat ("Failed to create tx: "%build) err
            Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    gen = makeManyAddressesToManyParams 1 1000000 1 1

stabilizationDoesNotFailSpec :: HasTxpConfigurations => TxpTestProperty ()
stabilizationDoesNotFailSpec = do
    forAllM gen $ \(CreateMTxParams {..}) -> do
        txOrError <- createMTx cmpUtxo (getSignerFromList cmpSigners) cmpOutputs cmpAddrData
        case txOrError of
            Left err@FailedToStabilize -> stopProperty $ pretty err
            Left _                     -> return ()
            Right tx                   -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    gen = makeManyAddressesToManyParams 1 200000 1 1

feeIsNonzeroSpec :: HasTxpConfigurations => TxpTestProperty ()
feeIsNonzeroSpec = do
    forAllM gen $ \(CreateMTxParams {..}) -> do
        txOrError <- createMTx cmpUtxo (getSignerFromList cmpSigners) cmpOutputs cmpAddrData
        case txOrError of
            Left (NotEnoughMoney _) -> return ()
            Left err -> stopProperty $ pretty err
            Right _ -> stopProperty $
                "Transaction was created even though there were " <>
                    "not enough funds for the fee"
  where
    gen = makeManyAddressesToManyParams 1 100000 1 1

manyUtxoTo1Spec :: HasTxpConfigurations => TxpTestProperty ()
manyUtxoTo1Spec = do
    forAllM gen $ \(CreateMTxParams {..}) -> do
        txOrError <- createMTx cmpUtxo (getSignerFromList cmpSigners) cmpOutputs cmpAddrData
        case txOrError of
            Left err -> stopProperty $ pretty err
            Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    gen = makeManyUtxoTo1Params 10 100000 1

manyAddressesTo1Spec :: HasTxpConfigurations => TxpTestProperty ()
manyAddressesTo1Spec = do
    forAllM gen $ \(CreateMTxParams {..}) -> do
        txOrError <- createMTx cmpUtxo (getSignerFromList cmpSigners) cmpOutputs cmpAddrData
        case txOrError of
            Left err -> stopProperty $ pretty err
            Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    gen = makeManyAddressesToManyParams 10 100000 1 1

manyAddressesToManySpec :: HasTxpConfigurations => TxpTestProperty ()
manyAddressesToManySpec = do
    forAllM gen $ \(CreateMTxParams {..}) -> do
        txOrError <- createMTx cmpUtxo (getSignerFromList cmpSigners) cmpOutputs cmpAddrData
        case txOrError of
            Left err -> stopProperty $ pretty err
            Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    gen = makeManyAddressesToManyParams 10 100000 10 1

redemptionSpec :: HasTxpConfigurations => TxpTestProperty ()
redemptionSpec = do
    forAllM genParams $ \(CreateRedemptionTxParams {..}) -> do
        txOrError <- createRedemptionTx crpUtxo crpRsk crpOutputs
        case txOrError of
            Left err -> stopProperty $ pretty err
            Right _  -> return ()
  where
    genParams = do
        crpRsk <- arbitrary
        skTo   <- arbitrary

        let txOutAuxInput = generateRedeemTxOutAux 1 crpRsk
            txOutAuxOutput = generateTxOutAux 1 skTo
            crpUtxo = one (TxInUtxo (unsafeIntegerToTxId 0) 0, txOutAuxInput)
            crpOutputs = one txOutAuxOutput

        pure CreateRedemptionTxParams {..}

txWithRedeemOutputFailsSpec :: HasTxpConfigurations => TxpTestProperty ()
txWithRedeemOutputFailsSpec = do
    forAllM genParams $ \(CreateMTxParams {..}) -> do
        txOrError <- createMTx cmpUtxo (getSignerFromList cmpSigners) cmpOutputs cmpAddrData
        case txOrError of
            Left (OutputIsRedeem _) -> return ()
            Left err -> stopProperty $ pretty err
            Right _  -> stopProperty $
                sformat ("Transaction to a redeem address was created")
  where
    genParams = do
        txOutAuxOutput <- generateRedeemTxOutAux 1 <$> arbitrary
        params <- makeManyAddressesToManyParams 1 1000000 1 1
        pure params{ cmpOutputs = one txOutAuxOutput }

feeForManyAddressesSpec
    :: HasTxpConfigurations
    => Bool
    -> TxpTestProperty ()
feeForManyAddressesSpec manyAddrs =
    forAllM (choose (5, 20)) $
        \(Coeff . fromInteger -> feePolicySlope) ->
    forAllM (choose (10000, 100000)) $
        \(Coeff . fromInteger -> feePolicyConstTerm) ->
    forAllM (choose (10000, 100000)) $
        \toSpend ->
    forAllM (choose (1000, 10000)) $
        \perAddrAmount ->
    forAllM (mkParams 100 perAddrAmount toSpend) $
        \params ->
    do

    withTxFeePolicy feePolicyConstTerm feePolicySlope $ do
        -- tx builder should find this utxo to be enough for construction
        txOrError <- createTxWithParams params
        txAux <- case txOrError of
            Left err ->
                if isNotEnoughMoneyTxError err
                then stop Discard
                else stopProperty $ sformat ("On first attempt: "%build) err
            Right (txAux, _) ->
                return txAux

        -- even if utxo size is barely enough - fee stabilization should achieve
        -- success as well
        -- 'succ' is needed here because current algorithm may fail to stabilize fee if
        -- almost all money are spent
        let enoughInputs = succ . length . _txInputs $ taTx txAux
            utxo' = M.fromList . take enoughInputs . M.toList $ cmpUtxo params
            params' = params { cmpUtxo = utxo' }
        txOrError' <- createTxWithParams params'
        case txOrError' of
            Left err -> stopProperty $ sformat ("On second attempt: "%build) err
            Right _  -> return ()
  where
    createTxWithParams CreateMTxParams {..} =
        createMTx cmpUtxo (getSignerFromList cmpSigners) cmpOutputs cmpAddrData
    -- considering two corner cases of utxo outputs distribution
    mkParams
        | manyAddrs = makeManyAddressesTo1Params
        | otherwise = makeManyUtxoTo1Params

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Container for parameters of `createMTx`.
data CreateMTxParams = CreateMTxParams
    { cmpUtxo     :: !Utxo
    -- ^ Unspent transaction outputs.
    , cmpSigners  :: !(NonEmpty (SafeSigner, Address))
    -- ^ Wrappers around secret keys for addresses in Utxo.
    , cmpOutputs  :: !TxOutputs
    -- ^ A (nonempty) list of desired tx outputs.
    , cmpAddrData :: !(AddrData TxpTestMode)
    -- ^ Data that is normally used for creation of change addresses.
    -- In tests, it is always `()`.
    } deriving Show

-- | Container for parameters of `createRedemptionTx`.
-- The parameters mirror those of `createMTx` almost perfectly.
data CreateRedemptionTxParams = CreateRedemptionTxParams
    { crpUtxo    :: !Utxo
    , crpRsk     :: !RedeemSecretKey
    , crpOutputs :: !TxOutputs
    } deriving Show

getSignerFromList :: NonEmpty (SafeSigner, Address) -> (Address -> SafeSigner)
getSignerFromList (HM.fromList . map swap . toList -> hm) =
    \addr -> fromMaybe (error "Requested signer for unknown address") $ HM.lookup addr hm

makeManyUtxoTo1Params :: Int -> Integer -> Integer -> Gen CreateMTxParams
makeManyUtxoTo1Params numFrom amountEachFrom amountTo = do
    [skFrom, skTo] <- nonrepeating 2

    let txOutAuxInput  = generateTxOutAux amountEachFrom skFrom
        txOutAuxOutput = generateTxOutAux amountTo skTo
        cmpUtxo = M.fromList
            [(TxInUtxo (unsafeIntegerToTxId 0) (fromIntegral k), txOutAuxInput) |
                k <- [0..numFrom-1]]
        cmpSigners = one $ makeSigner skFrom
        cmpOutputs = one txOutAuxOutput
        cmpAddrData = ()

    pure CreateMTxParams {..}

makeManyAddressesToManyParams
    :: Int
    -> Integer
    -> Int
    -> Integer
    -> Gen CreateMTxParams
makeManyAddressesToManyParams numFrom amountEachFrom numTo amountEachTo = do
    sks <- nonrepeating (numFrom + numTo)

    let (sksFrom, sksTo) = splitAt numFrom sks
        cmpSignersList = map makeSigner sksFrom
        cmpSigners = NE.fromList cmpSignersList
        txOutAuxInputs = map (generateTxOutAux amountEachFrom) sksFrom
        txOutAuxOutputs = map (generateTxOutAux amountEachTo) sksTo
        cmpUtxo = M.fromList
            [(TxInUtxo (unsafeIntegerToTxId $ fromIntegral k) 0, txOutAux) |
                (k, txOutAux) <- zip [0..numFrom-1] txOutAuxInputs]
        cmpOutputs = NE.fromList txOutAuxOutputs
        cmpAddrData = ()

    pure CreateMTxParams {..}

makeManyAddressesTo1Params :: Int -> Integer -> Integer -> Gen CreateMTxParams
makeManyAddressesTo1Params numFrom amountEachFrom amountEachTo =
    makeManyAddressesToManyParams numFrom amountEachFrom 1 amountEachTo

ensureTxMakesSense
  :: HasTxpConfigurations
  => TxWithSpendings -> Utxo -> TxOutputs -> TxpTestProperty ()
ensureTxMakesSense (_, neTxOut) utxo _ = do
    unless (S.fromList txOutUsed `S.isSubsetOf` S.fromList txOutAvailable) $
        stopProperty $
            sformat ("Used some inputs that were not available!\n"%
                    "Available: "%shown%"\n"%
                    "Used: "%shown
                    ) txOutAvailable txOutUsed
  where
    txOutAvailable = map toaOut $ M.elems utxo
    txOutUsed = NE.toList neTxOut

unsafeIntegerToTxId :: Integer -> TxId
unsafeIntegerToTxId n =
    leftToPanic "unsafeIntegerToTxId: " $ decodeHash $
        sformat (left 64 '0' %. hex) n

makeTxOutAux :: Integer -> Address -> TxOutAux
makeTxOutAux amount addr =
    let coin = unsafeIntegerToCoin amount
        txOut = TxOut addr coin
    in TxOutAux txOut

generateTxOutAux :: Integer -> SecretKey -> TxOutAux
generateTxOutAux amount sk =
    makeTxOutAux amount (secretKeyToAddress sk)

generateRedeemTxOutAux :: Integer -> RedeemSecretKey -> TxOutAux
generateRedeemTxOutAux amount rsk =
    makeTxOutAux amount (makeRedeemAddress $ redeemToPublic rsk)

secretKeyToAddress :: SecretKey -> Address
secretKeyToAddress = makePubKeyAddressBoot . toPublic

makeSigner :: SecretKey -> (SafeSigner, Address)
makeSigner sk = (fakeSigner sk, secretKeyToAddress sk)

withTxFeePolicy
  :: HasTxpConfigurations
  => Coeff -> Coeff -> TxpTestProperty () -> TxpTestProperty ()
withTxFeePolicy a b action = do
    let policy = TxFeePolicyTxSizeLinear $ TxSizeLinear a b
    bvd <- gsAdoptedBVData
    withBVData bvd{ bvdTxFeePolicy = policy } action
