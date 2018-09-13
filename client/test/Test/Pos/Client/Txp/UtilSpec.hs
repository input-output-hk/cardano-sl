{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Specification of Pos.Client.Txp.Util

module Test.Pos.Client.Txp.UtilSpec
       ( spec
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import           Formatting (build, hex, left, sformat, shown, (%), (%.))
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Discard (..), Gen, Testable, arbitrary,
                     choose)
import           Test.QuickCheck.Monadic (forAllM, stop)

import           Pos.Chain.Txp (Tx (..), TxAux (..), TxId, TxIn (..),
                     TxOut (..), TxOutAux (..), Utxo)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Util (InputSelectionPolicy (..), TxError (..),
                     TxOutputs, TxWithSpendings, createMTx, createRedemptionTx,
                     isNotEnoughMoneyTxError)
import           Pos.Core (Address, Coeff (..), TxFeePolicy (..),
                     TxSizeLinear (..), makePubKeyAddressBoot,
                     makeRedeemAddress, unsafeIntegerToCoin)
import           Pos.Core.Update (BlockVersionData (..))
import           Pos.Crypto (RedeemSecretKey, SafeSigner, SecretKey, decodeHash,
                     fakeSigner, redeemToPublic, toPublic)
import           Pos.DB (gsAdoptedBVData)
import           Pos.Util.Util (leftToPanic)

import           Test.Pos.Client.Txp.Mode (TxpTestMode, TxpTestProperty,
                     withBVData)
import           Test.Pos.Configuration (withDefConfigurations)
import           Test.Pos.Core.Dummy (dummyConfig)
import           Test.Pos.Crypto.Arbitrary ()
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)
import           Test.Pos.Util.QuickCheck.Arbitrary (nonrepeating)
import           Test.Pos.Util.QuickCheck.Property (stopProperty)

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

spec :: Spec
spec = withDefConfigurations $ \_ _ _ ->
    describe "Client.Txp.Util" $ do
        describe "createMTx" $ createMTxSpec

-- GHC doesn't support impredicative polymorphism so we need a wrapper
-- for the list below to typecheck.
data TestFunctionWrapper
    = forall prop. (Testable prop) => TestFunctionWrapper (InputSelectionPolicy -> prop)

createMTxSpec :: Spec
createMTxSpec = do
    let inputSelectionPolicies =
            [ ("Grouped inputs", OptimizeForSecurity)
            , ("Ungrouped inputs", OptimizeForHighThroughput)
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

testCreateMTx
    :: CreateMTxParams
    -> TxpTestProperty (Either TxError (TxAux, NonEmpty TxOut))
testCreateMTx CreateMTxParams {..} = lift $
    createMTx dummyConfig mempty cmpInputSelectionPolicy cmpUtxo (getSignerFromList cmpSigners)
    cmpOutputs cmpAddrData

createMTxWorksWhenWeAreRichSpec
    :: InputSelectionPolicy
    -> TxpTestProperty ()
createMTxWorksWhenWeAreRichSpec inputSelectionPolicy =
    forAllM gen $ \txParams@CreateMTxParams{..} -> do
        txOrError <- testCreateMTx txParams
        case txOrError of
            Left err -> stopProperty $ sformat ("Failed to create tx: "%build) err
            Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    gen = makeManyAddressesToManyParams inputSelectionPolicy 1 1000000 1 1

stabilizationDoesNotFailSpec
    :: InputSelectionPolicy
    -> TxpTestProperty ()
stabilizationDoesNotFailSpec inputSelectionPolicy = do
    forAllM gen $ \txParams@CreateMTxParams{..} -> do
        txOrError <- testCreateMTx txParams
        case txOrError of
            Left err@FailedToStabilize -> stopProperty $ pretty err
            Left _                     -> return ()
            Right tx                   -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    gen = makeManyAddressesToManyParams inputSelectionPolicy 1 200000 1 1

feeIsNonzeroSpec
    :: InputSelectionPolicy
    -> TxpTestProperty ()
feeIsNonzeroSpec inputSelectionPolicy = do
    forAllM gen $ \txParams@CreateMTxParams{..} -> do
        txOrError <- testCreateMTx txParams
        case txOrError of
            Left (NotEnoughMoney _) -> return ()
            Left err -> stopProperty $ pretty err
            Right _ -> stopProperty $
                "Transaction was created even though there were " <>
                    "not enough funds for the fee"
  where
    gen = makeManyAddressesToManyParams inputSelectionPolicy 1 100000 1 1

manyUtxoTo1Spec
    :: InputSelectionPolicy
    -> TxpTestProperty ()
manyUtxoTo1Spec inputSelectionPolicy = do
    forAllM gen $ \txParams@CreateMTxParams{..} -> do
        txOrError <- testCreateMTx txParams
        case txOrError of
            Left err -> stopProperty $ pretty err
            Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    gen = makeManyUtxoTo1Params inputSelectionPolicy 10 100000 1

manyAddressesTo1Spec
    :: InputSelectionPolicy
    -> TxpTestProperty ()
manyAddressesTo1Spec inputSelectionPolicy = do
    forAllM gen $ \txParams@CreateMTxParams{..} -> do
        txOrError <- testCreateMTx txParams
        case txOrError of
            Left err -> stopProperty $ pretty err
            Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    gen = makeManyAddressesToManyParams inputSelectionPolicy 10 100000 1 1

manyAddressesToManySpec
    :: InputSelectionPolicy
    -> TxpTestProperty ()
manyAddressesToManySpec inputSelectionPolicy = do
    forAllM gen $ \txParams@CreateMTxParams{..} -> do
        txOrError <- testCreateMTx txParams
        case txOrError of
            Left err -> stopProperty $ pretty err
            Right tx -> ensureTxMakesSense tx cmpUtxo cmpOutputs
  where
    gen = makeManyAddressesToManyParams inputSelectionPolicy 10 100000 10 1

redemptionSpec :: TxpTestProperty ()
redemptionSpec = do
    forAllM genParams $ \(CreateRedemptionTxParams {..}) -> do
        txOrError <- createRedemptionTx dummyProtocolMagic crpUtxo crpRsk crpOutputs
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

txWithRedeemOutputFailsSpec
    :: InputSelectionPolicy
    -> TxpTestProperty ()
txWithRedeemOutputFailsSpec inputSelectionPolicy = do
    forAllM genParams $ \(CreateMTxParams {..}) -> do
        txOrError <-
            createMTx dummyConfig mempty cmpInputSelectionPolicy cmpUtxo
                      (getSignerFromList cmpSigners)
                      cmpOutputs cmpAddrData
        case txOrError of
            Left (OutputIsRedeem _) -> return ()
            Left err -> stopProperty $ pretty err
            Right _  -> stopProperty $
                sformat ("Transaction to a redeem address was created")
  where
    genParams = do
        txOutAuxOutput <- generateRedeemTxOutAux 1 <$> arbitrary
        params <- makeManyAddressesToManyParams inputSelectionPolicy 1 1000000 1 1
        pure params{ cmpOutputs = one txOutAuxOutput }

feeForManyAddressesSpec
    :: InputSelectionPolicy
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
    forAllM (mkParams 100 perAddrAmount toSpend) $
        \params ->
    withTxFeePolicy feePolicyConstTerm feePolicySlope $ do
        -- tx builder should find this utxo to be enough for construction
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
        | manyAddrs = makeManyAddressesTo1Params inputSelectionPolicy
        | otherwise = makeManyUtxoTo1Params inputSelectionPolicy


groupedPolicySpec :: TxpTestProperty ()
groupedPolicySpec =
    forAllM gen $ testCreateMTx >=> \case
        Left err -> stopProperty $ pretty err
        Right (txAux, _) ->
            let picked = length . _txInputs . taTx $ txAux
            in unless (picked == utxoNum) . stopProperty
            $ sformat ("Only "%build%" inputs were used instead of all of the inputs") picked
  where
    utxoNum = 10
    gen = makeManyUtxoTo1Params OptimizeForSecurity (fromIntegral utxoNum) 1000000 1

ungroupedPolicySpec :: TxpTestProperty ()
ungroupedPolicySpec =
    forAllM gen $ testCreateMTx >=> \case
        Left err -> stopProperty $ pretty err
        Right (txAux, _) ->
            let picked = length . _txInputs . taTx $ txAux
            in unless (picked == 1) . stopProperty
            $ sformat ("Only "%build%" inputs were used instead of just 1 input") picked
  where
    gen = makeManyUtxoTo1Params OptimizeForHighThroughput 10 1000000 1

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
    } deriving Show

-- | Container for parameters of `createRedemptionTx`.
-- The parameters mirror those of `createMTx` almost perfectly.
data CreateRedemptionTxParams = CreateRedemptionTxParams
    { crpUtxo    :: !Utxo
    , crpRsk     :: !RedeemSecretKey
    , crpOutputs :: !TxOutputs
    } deriving Show

getSignerFromList :: NonEmpty (SafeSigner, Address) -> Address -> Maybe SafeSigner
getSignerFromList (HM.fromList . map swap . toList -> hm) =
    \addr -> HM.lookup addr hm

makeManyUtxoTo1Params :: InputSelectionPolicy -> Int -> Integer -> Integer -> Gen CreateMTxParams
makeManyUtxoTo1Params inputSelectionPolicy numFrom amountEachFrom amountTo = do
    ~[skFrom, skTo] <- nonrepeating 2
    let txOutAuxInput  = generateTxOutAux amountEachFrom skFrom
        txOutAuxOutput = generateTxOutAux amountTo skTo
        cmpInputSelectionPolicy = inputSelectionPolicy
        cmpUtxo = M.fromList
            [(TxInUtxo (unsafeIntegerToTxId 0) (fromIntegral k), txOutAuxInput) |
                k <- [0..numFrom-1]]
        cmpSigners = one $ makeSigner skFrom
        cmpOutputs = one txOutAuxOutput
        cmpAddrData = ()

    pure CreateMTxParams {..}

makeManyAddressesToManyParams
    :: InputSelectionPolicy
    -> Int
    -> Integer
    -> Int
    -> Integer
    -> Gen CreateMTxParams
makeManyAddressesToManyParams inputSelectionPolicy numFrom amountEachFrom numTo amountEachTo = do
    sks <- nonrepeating (numFrom + numTo)

    let (sksFrom, sksTo) = splitAt numFrom sks
        cmpSignersList = map makeSigner sksFrom
        cmpSigners = NE.fromList cmpSignersList
        txOutAuxInputs = map (generateTxOutAux amountEachFrom) sksFrom
        txOutAuxOutputs = map (generateTxOutAux amountEachTo) sksTo
        cmpInputSelectionPolicy = inputSelectionPolicy
        cmpUtxo = M.fromList
            [(TxInUtxo (unsafeIntegerToTxId $ fromIntegral k) 0, txOutAux) |
                (k, txOutAux) <- zip [0..numFrom-1] txOutAuxInputs]
        cmpOutputs = NE.fromList txOutAuxOutputs
        cmpAddrData = ()

    pure CreateMTxParams {..}

makeManyAddressesTo1Params :: InputSelectionPolicy -> Int -> Integer -> Integer -> Gen CreateMTxParams
makeManyAddressesTo1Params inputSelectionPolicy numFrom amountEachFrom amountEachTo =
    makeManyAddressesToManyParams inputSelectionPolicy numFrom amountEachFrom 1 amountEachTo

ensureTxMakesSense
  :: TxWithSpendings -> Utxo -> TxOutputs -> TxpTestProperty ()
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
  :: Coeff -> Coeff -> TxpTestProperty () -> TxpTestProperty ()
withTxFeePolicy a b action = do
    let policy = TxFeePolicyTxSizeLinear $ TxSizeLinear a b
    bvd <- gsAdoptedBVData
    withBVData bvd{ bvdTxFeePolicy = policy } action
