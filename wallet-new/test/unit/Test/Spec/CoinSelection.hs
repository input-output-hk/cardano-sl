{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Spec.CoinSelection (
    spec
  ) where

import           Universum

import qualified Data.List
import           Data.List.NonEmpty (cons)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Formatting (sformat)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Gen, Property, arbitrary, choose, conjoin, counterexample, forAll,
                                  suchThat)

import           Data.Text.Buildable (Buildable (..))
import           Formatting (bprint, (%))
import qualified Formatting as F
import           Serokell.Util.Text (listJsonIndent)
import qualified Text.Tabl as Tabl

import           Pos.Core (HasConfiguration)
import qualified Pos.Core as Core
import qualified Pos.Txp as Core

import           Util.Buildable

import           Cardano.Wallet.Kernel.CoinSelection.Policies (defaultPolicy, largestFirst,
                                                               utxoBalance)
import           Cardano.Wallet.Kernel.CoinSelection.Types
import           Pos.Crypto.Signing.Safe (fakeSigner)

data StakeGenOptions = StakeGenOptions {
      stakeOnEachInputPercentage :: Maybe Double
      -- ^ How much of the total stake each input should hold. For example,
      -- passing 1.0 would mean that 100% of the stake will be allocated on a
      -- single input, which would make running the coin selection policy harder
      -- with multiple outputs. If not specified, the generator will pick a
      -- random value in (0.0,1.0] for each generated output.
    , stakeGenerationTarget      :: GenerationTarget
    -- ^ How close we want to hit our target.
    , stakeNeeded                :: Core.Coin
    -- ^ How much stake we want to generate
    , stakeMaxInputsNum          :: Maybe Int
    -- ^ If specified, stop generating inputs if they exceed the supplied
    -- number, to keep the Utxo size under control.
}

data GenerationTarget =
      AtLeast
    -- ^ Generate an 'Utxo' which has @at least@ Core.Coin stake.
    | Exactly
    -- ^ Generate an 'Utxo' which has @exactly@ Core.Coin stake.
    deriving Eq

genUtxo :: StakeGenOptions -> Gen Core.Utxo
genUtxo o = go mempty (stakeNeeded o) o
    where
        finalise :: Core.Utxo -> Core.Coin -> Gen Core.Utxo
        finalise utxo amountToCover =
            case amountToCover `Core.subCoin` (utxoBalance utxo) of
                 Nothing -> return utxo
                 Just (Core.Coin 0) -> return utxo
                 Just remaining -> do
                     txIn <- Core.TxInUtxo <$> arbitrary <*> arbitrary
                     addr <- arbitrary
                     let txOutAux = Core.TxOutAux (Core.TxOut addr remaining)
                     return $ Map.insert txIn txOutAux utxo

        go :: Core.Utxo -> Core.Coin -> StakeGenOptions -> Gen Core.Utxo
        go accUtxo amountToCover opts =
            case needToStop (Map.size accUtxo) (stakeMaxInputsNum opts) of
                 True  -> finalise accUtxo amountToCover
                 False -> do
                     stakePercentage <- case stakeOnEachInputPercentage opts of
                                            Nothing -> choose (0.1, 1.0)
                                            Just r  -> pure r
                     txIn <- (Core.TxInUtxo <$> arbitrary <*> arbitrary) `suchThat` (not . flip Map.member accUtxo)
                     let adjust c = ceiling $ ((fromIntegral (Core.coinToInteger c)) * stakePercentage)
                     coin <- Core.mkCoin <$> choose (1, adjust (stakeNeeded opts))
                     addr <- arbitrary
                     let txOutAux = Core.TxOutAux (Core.TxOut addr coin)
                     let utxo' = Map.insert txIn txOutAux accUtxo
                     case utxoBalance utxo' of
                         bal | bal >= stakeNeeded opts && stakeGenerationTarget opts == AtLeast -> return utxo'
                         bal | bal >= stakeNeeded opts && stakeGenerationTarget opts == Exactly ->
                                 -- Cover 'amountToCover' exactly, on the old Utxo.
                                 finalise accUtxo amountToCover
                             | otherwise ->
                                 case amountToCover `Core.subCoin` coin of
                                     Nothing        -> error "invariant violated!"
                                     Just remaining -> go utxo' remaining opts

genUtxoWithAtLeast :: Word64 -> Gen Core.Utxo
genUtxoWithAtLeast amountToCover =
    genUtxo $ StakeGenOptions {
                  stakeOnEachInputPercentage = Just 0.2
                , stakeGenerationTarget = AtLeast
                , stakeNeeded           = Core.mkCoin amountToCover
                , stakeMaxInputsNum     = Just 100
            }

-- | A fee-estimation policy which doesn't calculate any fee.
freeLunch :: Int -> NonEmpty Core.Coin -> Core.Coin
freeLunch _ _ = Core.mkCoin 0

-- | The smallest fee possible.
minFee :: Int -> NonEmpty Core.Coin -> Core.Coin
minFee _ _ = Core.mkCoin 1

-- | A simple linear fee proportional in the #inputs & #outputs.
linearFee :: Int -> NonEmpty Core.Coin -> Core.Coin
linearFee inputsLen outputs = Core.mkCoin (fromIntegral $ inputsLen + length outputs)

genUniqueChangeAddress :: Core.Utxo
                       -> NonEmpty Core.TxOut
                       -> Gen Core.Address
genUniqueChangeAddress (map (Core.txOutAddress . Core.toaOut . snd) . Map.toList -> utxo)
                       (map Core.txOutAddress . toList -> outputs) =
    arbitrary `suchThat` (\a -> not (inUtxo utxo a) &&
                                not (a `Data.List.elem` outputs) &&
                                not (Core.isRedeemAddress a)
                         )
    where
        inUtxo :: [Core.Address] -> Core.Address -> Bool
        inUtxo addrs a = a `Data.List.elem` addrs

genTxOut :: StakeGenOptions
         -> Gen (NonEmpty Core.TxOut)
genTxOut opts =
    if stakeNeeded opts == Core.mkCoin 0
       then error "invaliant violation! You cannot pay 0 coins."
       else do o <- genOne
               go (o :| [])
    where
        genOne :: Gen Core.TxOut
        genOne = do
            stakePercentage <- case stakeOnEachInputPercentage opts of
                                   Nothing -> choose (0.1, 1.0)
                                   Just r  -> pure r
            let adjust c = ceiling $ fromIntegral (Core.coinToInteger c) * stakePercentage
            given <- Core.mkCoin <$> choose (1, adjust (stakeNeeded opts))
            addr  <- arbitrary `suchThat` (not . Core.isRedeemAddress)
            return $ Core.TxOut addr given

        finalise :: NonEmpty Core.TxOut -> Gen (NonEmpty Core.TxOut)
        finalise acc = do
            let slack = (stakeNeeded opts) `Core.unsafeSubCoin` (getTotal $ paymentAmount acc)
            addr  <- arbitrary `suchThat` (not . Core.isRedeemAddress)
            return $ (Core.TxOut addr slack) `cons` acc

        go :: NonEmpty Core.TxOut -> Gen (NonEmpty Core.TxOut)
        go acc = case needToStop (length acc) (stakeMaxInputsNum opts) of
            True  -> finalise acc
            False -> do
                o <- genOne
                let acc' = o `cons` acc
                case getTotal $ paymentAmount acc' of
                    bal | bal >= stakeNeeded opts && stakeGenerationTarget opts == AtLeast -> return acc'
                    bal | bal >= stakeNeeded opts && stakeGenerationTarget opts == Exactly ->
                            finalise acc
                        | otherwise -> go acc'

needToStop :: Int -> Maybe Int -> Bool
needToStop _ Nothing               = False
needToStop actual (Just requested) = actual >= requested

-- | For some reason the version of 'QuickCheck' we are using doesn't seem
-- to export 'withMaxSuccess'.
withMaxSuccess :: Int -> Spec -> Spec
withMaxSuccess x = modifyMaxSuccess (const x)

-- | Generates multiple payees.
genPayees :: Word64 -> Gen (NonEmpty Core.TxOut)
genPayees amountToCover =
    genTxOut StakeGenOptions {
               stakeOnEachInputPercentage = Just 0.15
             , stakeGenerationTarget = AtLeast
             , stakeNeeded           = Core.mkCoin amountToCover
             , stakeMaxInputsNum     = Just 5
             }

-- | Generates a single payee.
genPayee :: Word64 -> Gen (NonEmpty Core.TxOut)
genPayee amountToCover =
    genTxOut StakeGenOptions {
              stakeOnEachInputPercentage = Nothing
            , stakeGenerationTarget = AtLeast
            , stakeNeeded           = Core.mkCoin amountToCover
            , stakeMaxInputsNum     = Just 1
            }

instance (Buildable a, Buildable b) => Buildable (Either a b) where
    build (Right r) = bprint ("Right " % F.build) r
    build (Left l)  = bprint ("Left "  % F.build) l

instance Buildable [CoinSelectionFailure Core.Address] where
    build = bprint (listJsonIndent 4)

paymentAmount :: NonEmpty Core.TxOut -> TotalOutput
paymentAmount = TotalOutput . Core.unsafeIntegerToCoin
                            . Core.sumCoins
                            . map Core.txOutValue
                            . toList

{-------------------------------------------------------------------------------
  Rendering test failures in an helpful way to debug problems
-------------------------------------------------------------------------------}

type Cell = T.Text
type Row  = [Cell]

emptyCell :: Cell
emptyCell = mempty

renderZipped :: (a -> Row) -> (b -> Row) -> [a] -> [b] -> [Row]
renderZipped _ _ [] [] = []
renderZipped renderA renderB [] (x:xs) =
    (emptyCell : renderB x) : renderZipped renderA renderB [] xs
renderZipped renderA renderB (y:ys) [] =
    (renderA y  <> [mempty]) : renderZipped renderA renderB ys []
renderZipped renderA renderB (y:ys) (x:xs) =
    (renderA y <> renderB x) : renderZipped renderA renderB ys xs

renderUtxoAndPayees :: Core.Utxo -> NonEmpty Core.TxOut -> T.Text
renderUtxoAndPayees utxo outputs =
    Tabl.tabl Tabl.EnvAscii Tabl.DecorAll Tabl.DecorAll alignments cells
    where
      alignments :: [Tabl.Alignment]
      alignments = map (const Tabl.AlignCentre) cells

      cells :: [Row]
      cells =
          let header = ["UTXO", "Payment Amount"]
          in header : renderZipped toUtxoRow
                                   toPayeeRow
                                   (sortedUtxo $ Map.toList utxo)
                                   (sortedPayees $ toList outputs)

sortedUtxo :: [(Core.TxIn, Core.TxOutAux)]
           -> [(Core.TxIn, Core.TxOutAux)]
sortedUtxo = sortBy (\(_, t1) (_, t2) ->
                    (Core.getCoin . Core.txOutValue . Core.toaOut $ t2)
                    `compare`
                    (Core.getCoin . Core.txOutValue . Core.toaOut $ t1))

sortedPayees :: [Core.TxOut]  -> [Core.TxOut]
sortedPayees = sortBy (\p1 p2 ->
                      (Core.getCoin . Core.txOutValue $ p2)
                      `compare`
                      (Core.getCoin . Core.txOutValue $ p1))

toPayeeRow :: Core.TxOut -> Row
toPayeeRow txOut =
    [ T.take 8 (sformat F.build (Core.txOutAddress txOut))
     <> ":"
     <> (T.pack . show . Core.getCoin . Core.txOutValue $ txOut)
    ]

toUtxoRow :: (Core.TxIn, Core.TxOutAux) -> Row
toUtxoRow (_, txOutAux) =
    [T.pack . show . Core.getCoin . Core.txOutValue . Core.toaOut $ txOutAux]

-- | Render two outputs together, in a diff style.
renderOutputs :: NonEmpty Core.TxOut -> NonEmpty Core.TxOut -> Text
renderOutputs original actual =
    Tabl.tabl Tabl.EnvAscii Tabl.DecorAll Tabl.DecorAll alignments cells
    where
      alignments :: [Tabl.Alignment]
      alignments = map (const Tabl.AlignCentre) cells

      cells :: [Row]
      cells =
          let header = ["Original Outputs", "Outputs minus the fee"]
          in header : renderZipped toPayeeRow
                                   toPayeeRow
                                   (sortedPayees $ toList original)
                                   (sortedPayees $ toList actual)

{-------------------------------------------------------------------------------
  Handy combinators for property checking
-------------------------------------------------------------------------------}

-- Fail if the supplied predicate does not hold.
failIf :: Buildable a => String -> (a -> Bool) -> a -> Property
failIf label p a = counterexample msg (p a)
    where
        msg = label <> " triggered on " <> show (STB a)

paymentSucceeded :: (Buildable a, Buildable b)
                 => Core.Utxo -> NonEmpty Core.TxOut -> Either a b -> Property
paymentSucceeded utxo payees res =
    let msg = "Selection failed for Utxo with balance = " <> show (utxoBalance utxo) <>
              " and payment amount of " <> show (paymentAmount payees) <> "\n." <>
              "\n\n" <> T.unpack (renderUtxoAndPayees utxo payees) <>
              "\n\n"
    in failIf msg isRight res

paymentSucceededWith :: (Buildable a, Buildable b)
                     => Core.Utxo
                     -> NonEmpty Core.TxOut
                     -> Either a b
                     -> [Core.Utxo -> NonEmpty Core.TxOut -> b -> Property]
                     -> Property
paymentSucceededWith utxo payees res extraChecks =
    let msg = "Selection failed for Utxo with balance = " <> show (utxoBalance utxo) <>
              " and payment amount of " <> show (paymentAmount payees) <> "\n." <>
              "\n\n" <> T.unpack (renderUtxoAndPayees utxo payees) <>
              "\n\n"
    in case res of
            Left _  -> failIf msg isRight res
            Right b -> conjoin $ map (\f -> f utxo payees b) extraChecks

feeWasPayed :: ExpenseRegulation
            -> Core.Utxo
            -> NonEmpty Core.TxOut
            -> Core.TxAux
            -> Property
feeWasPayed SenderPaysFee _ _ _ = error "todo"
feeWasPayed ReceiverPaysFee _ originalOutputs tx =
    let txOutputs = Core._txOutputs . Core.taTx $ tx
        amended   = removeChangeAddresses originalOutputs txOutputs
    in failIf ( "final outputs value less than the original:\n\n"
                <> T.unpack (renderOutputs originalOutputs amended)
                <> "\n\n"
              )
              (\x -> x < (paymentAmount originalOutputs))
              (paymentAmount amended)

removeChangeAddresses :: NonEmpty Core.TxOut
                      -> NonEmpty Core.TxOut
                      -> NonEmpty Core.TxOut
removeChangeAddresses originalNE actualNE =
    let original = map Core.txOutAddress (toList originalNE)
        actual   = toList actualNE
    in case filter (not . isChangeAddress original) actual of
         []     -> error $ "removeChangeAddresses: invariant violated, no outputs found.\n\n"
                         <> (renderOutputs originalNE actualNE) <> "\n\n"
         (x:xs) -> x :| xs
    where
        isChangeAddress :: [Core.Address] -> Core.TxOut -> Bool
        isChangeAddress original x =
            not (Core.txOutAddress x `Data.List.elem` original)


{-------------------------------------------------------------------------------
  Combinators to assemble properties easily
-------------------------------------------------------------------------------}

type Policy =  CoinSelectionOptions
            -> Gen Core.Address
            -> Core.Utxo
            -> NonEmpty Core.TxOut
            -> Gen (Either [CoinSelectionFailure Core.Address] Core.TxAux)

type RunResult = ( Core.Utxo
                 , NonEmpty Core.TxOut
                 , Either (ShowThroughBuild [CoinSelectionFailure Core.Address]) Core.TxAux
                 )

newtype InitialBalance = InitialBalance Word64
newtype Pay = Pay Word64

pay :: (Word64 -> Gen Core.Utxo)
    -> (Word64 -> Gen (NonEmpty Core.TxOut))
    -> (Int -> NonEmpty Core.Coin -> Core.Coin)
    -> (CoinSelectionOptions -> CoinSelectionOptions)
    -> InitialBalance
    -> Pay
    -> Policy
    -> Gen RunResult
pay genU genP feeFunction adjustOptions (InitialBalance bal) (Pay amount) policy = do
    utxo  <- genU bal
    payee <- genP amount
    key   <- arbitrary
    let options = adjustOptions (newOptions feeFunction (\_ -> Right $ fakeSigner key))
    res <- bimap STB identity <$> policy options (genUniqueChangeAddress utxo payee) utxo payee
    return (utxo, payee, res)

payOne :: (Int -> NonEmpty Core.Coin -> Core.Coin)
       -> (CoinSelectionOptions -> CoinSelectionOptions)
       -> InitialBalance
       -> Pay
       -> Policy
       -> Gen RunResult
payOne = pay genUtxoWithAtLeast genPayee

payBatch :: (Int -> NonEmpty Core.Coin -> Core.Coin)
         -> (CoinSelectionOptions -> CoinSelectionOptions)
         -> InitialBalance
         -> Pay
         -> Policy
         -> Gen RunResult
payBatch = pay genUtxoWithAtLeast genPayees

receiverPays :: CoinSelectionOptions -> CoinSelectionOptions
receiverPays o = o & csoExpenseRegulation .~ ReceiverPaysFee

spec :: HasConfiguration => Spec
spec =
    describe "Coin selection policies unit tests" $ do
        withMaxSuccess 1000 $ describe "largestFirst" $ do
            prop "one payee, SenderPaysFee, fee = 0" $ forAll (
                 payOne freeLunch identity (InitialBalance 1000) (Pay 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "one payee, ReceiverPaysFee, fee = 0" $ forAll (
                 payOne freeLunch receiverPays (InitialBalance 1000) (Pay 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, SenderPaysFee, fee = 0" $ forAll (
                 payBatch freeLunch identity (InitialBalance 1000) (Pay 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, ReceiverPaysFee, fee = 0" $ forAll (
                 payBatch freeLunch receiverPays (InitialBalance 1000) (Pay 100) largestFirst
                 ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res

            -- Minimal fee
            prop "one payee, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payOne minFee identity (InitialBalance 1000) (Pay 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "one payee, ReceiverPaysFee, fee = 1 Lovelace" $ forAll (
                payOne minFee receiverPays (InitialBalance 1000) (Pay 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payBatch minFee identity (InitialBalance 1000) (Pay 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, ReceiverPaysFee, fee = 1 Lovelace" $ forAll (
                payBatch minFee receiverPays (InitialBalance 1000) (Pay 100) largestFirst
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res

        withMaxSuccess 1000 $ describe "defaultPolicy" $ do
            prop "one payee, SenderPaysFee, fee = 0" $ forAll (
                payOne freeLunch identity (InitialBalance 1000) (Pay 100) defaultPolicy
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "one payee, ReceiverPaysFee, fee = 0" $ forAll (
                payOne freeLunch receiverPays (InitialBalance 1000) (Pay 100) defaultPolicy
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, SenderPaysFee, fee = 0" $ forAll (
                payBatch freeLunch identity (InitialBalance 1000) (Pay 100) defaultPolicy
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, ReceiverPaysFee, fee = 0" $ forAll (
                payBatch freeLunch receiverPays (InitialBalance 1000) (Pay 100) defaultPolicy
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res

            -- minimal fee. It doesn't make sense to use it for 'ReceiverPaysFee', because
            -- rounding will essentially cause the computed @epsilon@ will be 0 for each
            -- output. For those cases, we use the 'linear' fee policy.
            prop "one payee, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payOne minFee identity (InitialBalance 1000) (Pay 100) defaultPolicy
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "one payee, ReceiverPaysFee, fee = linear" $ forAll (
                payOne linearFee receiverPays (InitialBalance 1000) (Pay 100) defaultPolicy
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed ReceiverPaysFee]
            prop "multiple payees, SenderPaysFee, fee = 1 Lovelace" $ forAll (
                payBatch minFee identity (InitialBalance 1000) (Pay 100) defaultPolicy
                ) $ \(utxo, payee, res) -> paymentSucceeded utxo payee res
            prop "multiple payees, ReceiverPaysFee, fee = linear" $ forAll (
                payBatch linearFee receiverPays (InitialBalance 1000) (Pay 100) defaultPolicy
                ) $ \(utxo, payee, res) ->
                  paymentSucceededWith utxo payee res [feeWasPayed ReceiverPaysFee]
