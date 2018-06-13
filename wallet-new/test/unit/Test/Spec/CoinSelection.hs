{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Test.Spec.CoinSelection (
    spec
  ) where

import           Universum

import           Crypto.Random (MonadRandom)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.List.NonEmpty (cons)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Gen, Property, arbitrary, choose, counterexample, suchThat)
import           Test.QuickCheck.Monadic (PropertyM, monadic', pick, run, stop)

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
        needToStop :: Int -> Maybe Int -> Bool
        needToStop _ Nothing               = False
        needToStop actual (Just requested) = actual >= requested

        finalise :: Core.Utxo -> Core.Coin -> Gen Core.Utxo
        finalise utxo amountToCover = do
            case amountToCover `Core.subCoin` (utxoBalance utxo) of
                 Nothing -> return utxo
                 Just (Core.Coin 0) -> return utxo
                 Just remaining -> do
                     txIn <- Core.TxInUtxo <$> arbitrary <*> arbitrary
                     addr <- arbitrary
                     let txOutAux = Core.TxOutAux (Core.TxOut addr remaining)
                     return $ Map.insert txIn txOutAux utxo

        go :: Core.Utxo -> Core.Coin -> StakeGenOptions -> Gen Core.Utxo
        go accUtxo amountToCover opts = do
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
                         bal | bal >= (stakeNeeded opts) && (stakeGenerationTarget opts) == AtLeast -> return utxo'
                         bal | bal >= (stakeNeeded opts) && (stakeGenerationTarget opts) == Exactly ->
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

genTxOut :: StakeGenOptions
         -> Gen (NonEmpty Core.TxOut)
genTxOut opts =
    if (stakeNeeded opts) == Core.mkCoin 0
       then error "invaliant violation! You cannot pay 0 coins."
       else do o <- genOne
               go (o :| [])
    where

        genOne :: Gen Core.TxOut
        genOne = do
            stakePercentage <- case stakeOnEachInputPercentage opts of
                                   Nothing -> choose (0.1, 1.0)
                                   Just r  -> pure r
            let adjust c = ceiling $ ((fromIntegral (Core.coinToInteger c)) * stakePercentage)
            given <- Core.mkCoin <$> choose (1, adjust (stakeNeeded opts))
            addr  <- arbitrary `suchThat` (not . Core.isRedeemAddress)
            return $ Core.TxOut addr given

        needToStop :: Int -> Maybe Int -> Bool
        needToStop _ Nothing               = False
        needToStop actual (Just requested) = actual >= requested

        finalise :: NonEmpty Core.TxOut -> Gen (NonEmpty Core.TxOut)
        finalise acc = do
            let slack = (stakeNeeded opts) `Core.unsafeSubCoin` (getTotal $ paymentAmount acc)
            addr  <- arbitrary `suchThat` (not . Core.isRedeemAddress)
            return $ (Core.TxOut addr slack) `cons` acc

        go :: NonEmpty Core.TxOut -> Gen (NonEmpty Core.TxOut)
        go acc = do
            case needToStop (length acc) (stakeMaxInputsNum opts) of
                 True  -> finalise acc
                 False -> do
                     o <- genOne
                     let acc' = o `cons` acc
                     case getTotal $ paymentAmount acc' of
                         bal | bal >= (stakeNeeded opts) && (stakeGenerationTarget opts) == AtLeast -> return acc'
                         bal | bal >= (stakeNeeded opts) && (stakeGenerationTarget opts) == Exactly ->
                                 finalise acc
                             | otherwise -> go acc'

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

newtype TestMonad a = TM { runTest :: IdentityT Gen a }
  deriving (Functor, Applicative, Monad, MonadRandom)

runIt :: PropertyM TestMonad a -> Gen Property
runIt m = monadic' m >>= runIdentityT . runTest

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

type Cell = T.Text
type Row  = [Cell]

renderUtxoAndPayees :: Core.Utxo -> NonEmpty Core.TxOut -> T.Text
renderUtxoAndPayees utxo outputs = Tabl.tabl env hDec vDec alignments cells
    where
      emptyCell :: Cell
      emptyCell = mempty

      env :: Tabl.Environment
      env = Tabl.EnvAscii

      hDec :: Tabl.Decoration
      hDec = Tabl.DecorAll

      vDec :: Tabl.Decoration
      vDec = Tabl.DecorAll

      zippedData :: [(Core.TxIn, Core.TxOutAux)] -> [Core.TxOut] -> [Row]
      zippedData [] [] = []
      zippedData [] (x:xs) = (emptyCell : toPayeeCell x) : zippedData [] xs
      zippedData (y:ys) [] = (toUtxoCell y  <> [mempty]) : zippedData ys []
      zippedData (y:ys) (x:xs) = 
          (toUtxoCell y <> toPayeeCell x) : zippedData ys xs

      alignments :: [Tabl.Alignment]
      alignments = map (const Tabl.AlignCentre) cells

      cells :: [Row]
      cells = ["UTXO", "Payment Amount"] : zippedData (sortedUtxo $ Map.toList utxo) (sortedPayees $ toList outputs)

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

      toPayeeCell :: Core.TxOut -> [T.Text]
      toPayeeCell txOut =
          [T.pack . show . Core.getCoin . Core.txOutValue $ txOut]

      toUtxoCell :: (Core.TxIn, Core.TxOutAux) -> [T.Text]
      toUtxoCell (_, txOutAux) =
          [T.pack . show . Core.getCoin . Core.txOutValue . Core.toaOut $ txOutAux]

type Policy =  CoinSelectionOptions
            -> TestMonad Core.Address
            -> Core.Utxo
            -> NonEmpty Core.TxOut
            -> TestMonad (Either [CoinSelectionFailure Core.Address] Core.TxAux)

type RunResult = (Core.Utxo, NonEmpty Core.TxOut, Either [CoinSelectionFailure Core.Address] Core.TxAux)

newtype InitialBalance = InitialBalance Word64
newtype Pay = Pay Word64

pay :: (Word64 -> Gen Core.Utxo)
    -> (Word64 -> Gen (NonEmpty Core.TxOut))
    -> (Int -> NonEmpty Core.Coin -> Core.Coin)
    -> (CoinSelectionOptions -> CoinSelectionOptions)
    -> InitialBalance
    -> Pay
    -> Policy
    -> PropertyM TestMonad RunResult
pay genU genP feeFunction adjustOptions (InitialBalance bal) (Pay amount) policy = do
    utxo  <- pick $ genU bal
    payee <- pick $ genP amount
    key   <- pick arbitrary
    let options = adjustOptions (newOptions feeFunction (\_ -> Right $ fakeSigner key))
    res <- run $ policy options (TM $ lift arbitrary) utxo payee
    return (utxo, payee, res)

payOne :: (Int -> NonEmpty Core.Coin -> Core.Coin)
       -> (CoinSelectionOptions -> CoinSelectionOptions)
       -> InitialBalance
       -> Pay
       -> Policy
       -> PropertyM TestMonad RunResult
payOne = pay genUtxoWithAtLeast genPayee

payBatch :: (Int -> NonEmpty Core.Coin -> Core.Coin)
         -> (CoinSelectionOptions -> CoinSelectionOptions)
         -> InitialBalance
         -> Pay
         -> Policy
         -> PropertyM TestMonad RunResult
payBatch = pay genUtxoWithAtLeast genPayees

spec :: HasConfiguration => Spec
spec =
    describe "Coin selection policies unit tests" $ do
        describe "largestFirst" $ do
            prop "one payee, SenderPaysFee, fee = 0" $ runIt $ do
                (utxo, payee, res) <- payOne freeLunch identity (InitialBalance 1000) (Pay 100) largestFirst
                stop (paymentSucceeded utxo payee res)
            prop "one payee, ReceiverPaysFee, fee = 0" $ runIt $ do
                let modOpt o = o & csoExpenseRegulation .~ ReceiverPaysFee
                (utxo, payee, res) <- payOne freeLunch modOpt (InitialBalance 1000) (Pay 100) largestFirst
                stop (paymentSucceeded utxo payee res)
            prop "multiple payees, SenderPaysFee, fee = 0" $ runIt $ do
                (utxo, payee, res) <- payBatch freeLunch identity (InitialBalance 1000) (Pay 100) largestFirst
                stop (paymentSucceeded utxo payee res)
            prop "multiple payees, ReceiverPaysFee, fee = 0" $ runIt $ do
                let modOpt o = o & csoExpenseRegulation .~ ReceiverPaysFee
                (utxo, payee, res) <- payBatch freeLunch modOpt (InitialBalance 1000) (Pay 100) largestFirst
                stop (paymentSucceeded utxo payee res)
        describe "defaultPolicy" $ do
            prop "one payee, SenderPaysFee, fee = 0" $ runIt $ do
                (utxo, payee, res) <- payOne freeLunch identity (InitialBalance 1000) (Pay 100) defaultPolicy
                stop (paymentSucceeded utxo payee res)
            prop "multiple payees, SenderPaysFee, fee = 0" $ runIt $ do
                (utxo, payee, res) <- payBatch freeLunch identity (InitialBalance 1000) (Pay 100) defaultPolicy
                stop (paymentSucceeded utxo payee res)
