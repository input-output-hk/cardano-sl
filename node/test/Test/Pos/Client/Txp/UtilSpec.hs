{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Specification of Pos.Client.Txp.Util

module Test.Pos.Client.Txp.UtilSpec
       ( spec
       ) where

import           Universum

import qualified Data.ByteString          as BS
import qualified Data.HashMap.Strict      as HM
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Formatting               (build, hex, left, sformat, shown, (%), (%.))
import           Test.Hspec               (Spec, describe)
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck          (Discard (..), Testable, choose)
import           Test.QuickCheck.Monadic  (forAllM, stop)

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Util      (InputSelectionPolicy (..), TxError (..),
                                           TxOutputs, TxWithSpendings, createMTx,
                                           createRedemptionTx, isNotEnoughMoneyTxError)
import           Pos.Core                 (BlockVersionData (..), Coeff (..),
                                           TxFeePolicy (..), TxSizeLinear (..),
                                           unsafeIntegerToCoin)
import           Pos.Crypto               (RedeemSecretKey, SafeSigner, SecretKey,
                                           decodeHash, fakeSigner)
import qualified Pos.GState               as GS
import           Pos.Txp                  (Tx (..), TxAux (..), TxId, TxIn (..),
                                           TxOut (..), TxOutAux (..), Utxo)
import           Pos.Types                (Address)
import qualified Pos.Update.DB            as DB
import           Pos.Util.Util            (leftToPanic)
import           Test.Pos.Util            (giveCoreConf, giveGtConf, giveInfraConf,
                                           giveNodeConf, giveUpdateConf, stopProperty)

import           Test.Pos.Client.Txp.Mode (HasTxpConfigurations, TxpTestMode,
                                           TxpTestProperty)
import           Test.Pos.Client.Txp.Util (generateAddressWithKey,
                                           generateRedeemAddressWithKey, seedSize)

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

spec :: Spec
spec = giveGtConf $ giveNodeConf $ giveInfraConf $ giveUpdateConf $ giveCoreConf $
    describe "Client.Txp.Util" $ do
        describe "createMTx" $ createMTxSpec

-- GHC doesn't support impredicative polymorphism so we need a wrapper
-- for the list below to typecheck.
data TestFunctionWrapper
    = forall prop. (Testable prop) => TestFunctionWrapper (InputSelectionPolicy -> prop)

createMTxSpec :: HasTxpConfigurations => Spec
createMTxSpec = do
    let inputSelectionPolicies =
            [ ("Grouped inputs", OptimizeForSecurity)
            , ("Ungrouped inputs", OptimizeForSize)
            ]
    let testSpecs =
            [ (createMTxWorksWhenWeAreRichDesc, TestFunctionWrapper createMTxWorksWhenWeAreRichSpec)
            , (stabilizationDoesNotFailDesc, TestFunctionWrapper stabilizationDoesNotFailSpec)
            , (feeIsNonzeroDesc, TestFunctionWrapper feeIsNonzeroSpec)
            , (manyUtxoTo1Desc, TestFunctionWrapper manyUtxoTo1Spec)
            , (manyAddressesTo1Desc, TestFunctionWrapper manyAddressesTo1Spec)
            , (manyAddressesToManyDesc, TestFunctionWrapper manyAddressesToManySpec)
            , (txWithRedeemOutputFailsDesc, TestFunctionWrapper txWithRedeemOutputFailsSpec)
            , (feeForManyAddressesDesc, TestFunctionWrapper feeForManyAddressesSpec)
            ]

    for_ inputSelectionPolicies $ \(inputSelectionDesc, policy) ->
        describe inputSelectionDesc . for_ testSpecs $ \(funcDesc, TestFunctionWrapper func) ->
            prop funcDesc (func policy)

    prop redemptionDesc redemptionSpec
    prop groupedPolicyDesc groupedPolicySpec
    prop ungroupedPolicyDesc ungroupedPolicySpec
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
    groupedPolicyDesc =
        "The amount of used inputs equals the amount of available inputs"
    ungroupedPolicyDesc =
        "The amount of used inputs is as small as possible"


getSignerFromList :: NonEmpty (SafeSigner, Address) -> (Address -> SafeSigner)
getSignerFromList (HM.fromList . map swap . toList -> hm) =
    \addr -> fromMaybe (error "Requested signer for unknown address") $ HM.lookup addr hm

testCreateMTx
    :: HasTxpConfigurations
    => CreateMTxParams
    -> TxpTestProperty (Either TxError (TxAux, NonEmpty TxOut))
testCreateMTx CreateMTxParams{..} =
    createMTx mempty cmpInputSelectionPolicy cmpUtxo (getSignerFromList cmpSigners)
    cmpOutputs cmpAddrData

createMTxWorksWhenWeAreRichSpec :: HasTxpConfigurations => InputSelectionPolicy -> TxpTestProperty ()
createMTxWorksWhenWeAreRichSpec inputSelectionPolicy = do
    txOrError <- testCreateMTx txParams
    case txOrError of
        Left err -> stopProperty $ sformat ("Failed to create tx: "%build) err
        Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    txParams@CreateMTxParams{..} = makeManyAddressesToManyParams inputSelectionPolicy 1 1000000 1 1

stabilizationDoesNotFailSpec :: HasTxpConfigurations => InputSelectionPolicy -> TxpTestProperty ()
stabilizationDoesNotFailSpec inputSelectionPolicy = do
    txOrError <- testCreateMTx txParams
    case txOrError of
        Left err@FailedToStabilize -> stopProperty $ pretty err
        Left _                     -> return ()
        Right tx                   -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    txParams@CreateMTxParams {..} = makeManyAddressesToManyParams inputSelectionPolicy 1 200000 1 1

feeIsNonzeroSpec :: HasTxpConfigurations => InputSelectionPolicy -> TxpTestProperty ()
feeIsNonzeroSpec inputSelectionPolicy = do
    txOrError <- testCreateMTx txParams
    case txOrError of
        Left (NotEnoughMoney _) -> return ()
        Left err -> stopProperty $ pretty err
        Right _ -> stopProperty $
            "Transaction was created even though there were " <>
                "not enough funds for the fee"
  where
    txParams@CreateMTxParams {..} = makeManyAddressesToManyParams inputSelectionPolicy 1 100000 1 1

manyUtxoTo1Spec :: HasTxpConfigurations => InputSelectionPolicy -> TxpTestProperty ()
manyUtxoTo1Spec inputSelectionPolicy = do
    txOrError <- testCreateMTx txParams
    case txOrError of
        Left err -> stopProperty $ pretty err
        Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    txParams@CreateMTxParams {..} = makeManyUtxoTo1Params inputSelectionPolicy 10 100000 1

manyAddressesTo1Spec :: HasTxpConfigurations => InputSelectionPolicy -> TxpTestProperty ()
manyAddressesTo1Spec inputSelectionPolicy = do
    txOrError <- testCreateMTx txParams
    case txOrError of
        Left err -> stopProperty $ pretty err
        Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    txParams@CreateMTxParams {..} = makeManyAddressesToManyParams inputSelectionPolicy 10 100000 1 1

manyAddressesToManySpec :: HasTxpConfigurations => InputSelectionPolicy -> TxpTestProperty ()
manyAddressesToManySpec inputSelectionPolicy = do
    txOrError <- testCreateMTx txParams
    case txOrError of
        Left err -> stopProperty $ pretty err
        Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    txParams@CreateMTxParams {..} = makeManyAddressesToManyParams inputSelectionPolicy 10 100000 10 1

redemptionSpec :: HasTxpConfigurations => TxpTestProperty ()
redemptionSpec = do
    txOrError <- createRedemptionTx utxo rsk outputs
    case txOrError of
        Left err -> stopProperty $ pretty err
        Right _  -> return ()
  where
    seedInput  = BS.replicate seedSize (0 :: Word8)
    seedOutput = BS.replicate seedSize (1 :: Word8)

    (rsk, txOutAuxInput)   = generateRedeemTxOutAux 1 seedInput
    (_, _, txOutAuxOutput) = generateTxOutAux 1 seedOutput

    utxo = one (TxInUtxo (unsafeIntegerToTxId 0) 0, txOutAuxInput)
    outputs = one txOutAuxOutput

txWithRedeemOutputFailsSpec :: HasTxpConfigurations => InputSelectionPolicy -> TxpTestProperty ()
txWithRedeemOutputFailsSpec inputSelectionPolicy = do
    txOrError <-
        createMTx mempty inputSelectionPolicy utxo (getSignerFromList signers) outputs addrData
    case txOrError of
        Left (OutputIsRedeem _) -> return ()
        Left err -> stopProperty $ pretty err
        Right _  -> stopProperty $
            sformat ("Transaction to a redeem address was created")
  where
    seedInput  = BS.replicate seedSize (0 :: Word8)
    seedOutput = BS.replicate seedSize (1 :: Word8)

    (sk, addr, txOutAuxInput) = generateTxOutAux 1000000 seedInput
    (_, txOutAuxOutput)       = generateRedeemTxOutAux 1 seedOutput

    utxo = one (TxInUtxo (unsafeIntegerToTxId 0) 0, txOutAuxInput)
    signers = one (fakeSigner sk, addr)
    outputs = one txOutAuxOutput
    addrData = ()

feeForManyAddressesSpec
    :: HasTxpConfigurations
    => InputSelectionPolicy
    -> Bool
    -> TxpTestProperty ()
feeForManyAddressesSpec inputSelectionPolicy manyAddrs =
    forAllM (choose (5, 20)) $
        \(Coeff . fromInteger -> feePolicySlope) ->
    forAllM (choose (10000, 100000)) $
        \(Coeff . fromInteger -> feePolicyConstTerm) ->
    forAllM (choose (10000, 100000)) $
        \toSpend ->
    forAllM (choose (1000, 10000)) $
        \perAddrAmount ->
    do
    setTxFeePolicy feePolicyConstTerm feePolicySlope

    -- tx builder should find this utxo to be enough for construction
    let params = mkParams inputSelectionPolicy 100 perAddrAmount toSpend
    txOrError <- testCreateMTx params
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
    txOrError' <- testCreateMTx params'
    case txOrError' of
        Left err -> stopProperty $ sformat ("On second attempt: "%build) err
        Right _  -> return ()
  where
    -- considering two corner cases of utxo outputs distribution
    mkParams
        | manyAddrs = makeManyAddressesTo1Params
        | otherwise = makeManyUtxoTo1Params


groupedPolicySpec :: HasTxpConfigurations => TxpTestProperty ()
groupedPolicySpec =
    testCreateMTx txParams >>= \case
        Left err -> stopProperty $ pretty err
        Right (txAux, _) ->
            let picked = length . _txInputs . taTx $ txAux
            in unless (picked == utxoNum) . stopProperty
            $ sformat ("Only "%build%" inputs were used instead of all of the inputs") picked
  where
    utxoNum = 10
    txParams = makeManyUtxoTo1Params OptimizeForSecurity (fromIntegral utxoNum) 1000000 1

ungroupedPolicySpec :: HasTxpConfigurations => TxpTestProperty ()
ungroupedPolicySpec =
    testCreateMTx txParams >>= \case
        Left err -> stopProperty $ pretty err
        Right (txAux, _) ->
            let picked = length . _txInputs . taTx $ txAux
            in unless (picked == 1) . stopProperty
            $ sformat ("Only "%build%" inputs were used instead of just 1 input") picked
  where
    txParams = makeManyUtxoTo1Params OptimizeForSize 10 1000000 1

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Container for parameters of `createMTx`.
data CreateMTxParams = CreateMTxParams
    { cmpInputSelectionPolicy :: InputSelectionPolicy
    -- ^ Input selection policy
    , cmpUtxo                 :: !Utxo
    -- ^ Unspent transaction outputs.
    , cmpSigners              :: !(NonEmpty (SafeSigner, Address))
    -- ^ Wrappers around secret keys for addresses in Utxo.
    , cmpOutputs              :: !TxOutputs
    -- ^ A (nonempty) list of desired tx outputs.
    , cmpAddrData             :: !(AddrData TxpTestMode)
    -- ^ Data that is normally used for creation of change addresses.
    -- In tests, it is always `()`.
    }

makeManyUtxoTo1Params :: InputSelectionPolicy -> Word8 -> Integer -> Integer -> CreateMTxParams
makeManyUtxoTo1Params inputSelectionPolicy numFrom amountEachFrom amountTo = CreateMTxParams {..}
  where
    seedInput  = BS.replicate seedSize (0 :: Word8)
    seedOutput = BS.replicate seedSize (1 :: Word8)

    (sk, addr, txOutAuxInput) = generateTxOutAux amountEachFrom seedInput
    (_, _, txOutAuxOutput)    = generateTxOutAux amountTo       seedOutput

    cmpInputSelectionPolicy = inputSelectionPolicy
    cmpUtxo = M.fromList
        [(TxInUtxo (unsafeIntegerToTxId 0) (fromIntegral k), txOutAuxInput) |
            k <- [0..numFrom-1]]
    cmpSigners = one (fakeSigner sk, addr)
    cmpOutputs = one txOutAuxOutput
    cmpAddrData = ()

makeManyAddressesToManyParams :: InputSelectionPolicy -> Word8 -> Integer -> Word8 -> Integer -> CreateMTxParams
makeManyAddressesToManyParams inputSelectionPolicy numFrom amountEachFrom numTo amountEachTo = CreateMTxParams {..}
  where
    seedsInput = [BS.replicate seedSize v | v <- [0..numFrom-1]]
    (cmpSignersList, txOutAuxInputs) = unzip
        [((fakeSigner sk, addr), txOutAux) |
            (sk, addr, txOutAux) <- map (generateTxOutAux amountEachFrom) seedsInput]

    cmpSigners = NE.fromList cmpSignersList

    seedsOutput = [BS.replicate seedSize v | v <- [numFrom..numFrom+numTo-1]]
    txOutAuxOutputs = [txOutAux |
        (_, _, txOutAux) <- map (generateTxOutAux amountEachTo) seedsOutput]

    cmpInputSelectionPolicy = inputSelectionPolicy
    cmpUtxo = M.fromList [(TxInUtxo (unsafeIntegerToTxId $ fromIntegral k) 0, txOutAux) |
        (k, txOutAux) <- zip [0..numFrom-1] txOutAuxInputs]
    cmpOutputs = NE.fromList txOutAuxOutputs
    cmpAddrData = ()

makeManyAddressesTo1Params :: InputSelectionPolicy -> Word8 -> Integer -> Integer -> CreateMTxParams
makeManyAddressesTo1Params inputSelectionPolicy numFrom amountEachFrom amountEachTo =
    makeManyAddressesToManyParams inputSelectionPolicy numFrom amountEachFrom 1 amountEachTo

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

generateTxOutAux :: Integer -> ByteString -> (SecretKey, Address, TxOutAux)
generateTxOutAux amount seed =
    let (sk, addr) = generateAddressWithKey seed
    in (sk, addr, makeTxOutAux amount addr)

generateRedeemTxOutAux :: Integer -> ByteString -> (RedeemSecretKey, TxOutAux)
generateRedeemTxOutAux amount seed =
    let (sk, addr) = generateRedeemAddressWithKey seed
    in (sk, makeTxOutAux amount addr)

setTxFeePolicy :: HasTxpConfigurations => Coeff -> Coeff -> TxpTestProperty ()
setTxFeePolicy a b = lift $ do
    let policy = TxFeePolicyTxSizeLinear $ TxSizeLinear a b
    (bv, bvd) <- DB.getAdoptedBVFull
    GS.writeBatchGState . one $ DB.SetAdopted bv bvd{ bvdTxFeePolicy = policy }
