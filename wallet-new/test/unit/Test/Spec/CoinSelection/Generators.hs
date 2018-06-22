{-# LANGUAGE ViewPatterns #-}
module Test.Spec.CoinSelection.Generators (
      genGroupedUtxo
    , genPayee
    , genPayees
    , genUtxo
    , InitialBalance(..)
    , Pay(..)
    , genUniqueChangeAddress
    , genUtxoWithAtLeast
    , genRedeemPayee
    ) where

import           Universum

import qualified Data.List
import qualified Data.Map as Map
import           Formatting (sformat)
import           Test.QuickCheck (Gen, arbitrary, choose, suchThat)

import qualified Formatting as F

import           Pos.Core ()
import qualified Pos.Core as Core
import           Pos.Crypto ()
import qualified Pos.Txp as Core

import           Util.Buildable ()

import           Cardano.Wallet.Kernel.CoinSelection ()
import           Cardano.Wallet.Kernel.Util (paymentAmount, utxoBalance)

{-------------------------------------------------------------------------------
  Useful types
-------------------------------------------------------------------------------}

data InitialBalance =
      InitialLovelace Word64
    | InitialADA Word64

data Pay =
      PayLovelace Word64
    | PayADA Word64

class ToLovelaces a where
    toLovelaces :: a -> Word64

instance ToLovelaces InitialBalance where
    toLovelaces (InitialLovelace w) = w
    toLovelaces (InitialADA w)      = w * 1000000

instance ToLovelaces Pay where
    toLovelaces (PayLovelace w) = w
    toLovelaces (PayADA w)      = w * 1000000

{-------------------------------------------------------------------------------
  Generating stake for Utxo & payments
-------------------------------------------------------------------------------}

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
    , stakeMaxEntries            :: Maybe Int
    -- ^ If specified, stop generating entries if they exceed the supplied
    -- number, to keep the Utxo size under control.
    , fiddlyAddresses            :: Bool
    -- ^ If set to 'True', the generator will try to produce addresses which
    -- base58 encoding is < 104 characters, as in the past this created problems
    -- to the coin selection algorithm.
    , allowRedeemAddresses       :: Bool
    }

data GenerationTarget =
      AtLeast
    -- ^ Generate an 'Utxo' which has @at least@ Core.Coin stake.
    | Exactly
    -- ^ Generate an 'Utxo' which has @exactly@ Core.Coin stake.
    deriving Eq

arbitraryAddress :: StakeGenOptions
                 -> Gen Core.Address
arbitraryAddress opts = do
    let fiddlyCondition a = if fiddlyAddresses opts
                               then length (sformat F.build a) < 104
                               else True
    let redeemCondition a = if allowRedeemAddresses opts
                               then True
                               else not (Core.isRedeemAddress a)
    arbitrary `suchThat` (\a -> fiddlyCondition a && redeemCondition a)


-- | Finalise the generation of 'a' by transferring all the remaining \"slack\".
finalise :: Semigroup a
         => (Maybe a -> Core.Coin -> Gen a)
         -- ^ A function to generate a value of type 'a', together with
         -- the currently accumulated value, if any.
         -> (a -> Core.Coin)
         -- ^ A function to extract monetary value out of 'a'.
         -> Core.Coin
         -- ^ The amount to cover
         -> a
         -- ^ The current accumulator
         -> Gen a
finalise genValue getValue amountToCover acc =
    case amountToCover `Core.subCoin` (getValue acc) of
         Nothing -> return acc
         Just (Core.Coin 0) -> return acc
         Just remaining -> do
             lastValue <- genValue (Just acc) remaining
             return $ acc <> lastValue

fromStakeOptions :: forall a. (Container a, Semigroup a)
                 => StakeGenOptions
                 -> (Maybe a -> Core.Coin -> Gen a)
                 -- ^ How to generate a singleton 'a'.
                 -> (a -> Core.Coin)
                 -- ^ A function to extract monetary value out of 'a'.
                 -> Gen a
fromStakeOptions o genValue getValue =
    if stakeMaxEntries o == Just 1
       then (genValue Nothing) (stakeNeeded o)
       else genCoin >>= genValue Nothing >>= go (stakeNeeded o)
    where
        genCoin :: Gen Core.Coin
        genCoin = do
            stakePercentage <- case stakeOnEachInputPercentage o of
                                   Nothing -> choose (0.1, 1.0)
                                   Just r  -> pure r
            let adjust c = ceiling $ ((fromIntegral (Core.coinToInteger c)) * stakePercentage)
            Core.mkCoin <$> choose (1, adjust (stakeNeeded o))

        needToStop :: Int -> Maybe Int -> Bool
        needToStop _ Nothing               = False
        needToStop actual (Just requested) = actual >= requested

        go :: Core.Coin -> a -> Gen a
        go amountToCover acc =
            case needToStop (length acc) (stakeMaxEntries o) of
                 True  -> finalise genValue getValue amountToCover acc
                 False -> do
                     coin <- genCoin
                     acc' <- ((<>) acc) <$> genValue (Just acc) coin
                     case getValue acc' of
                         bal | bal >= stakeNeeded o && stakeGenerationTarget o == AtLeast -> return acc'
                         bal | bal >= stakeNeeded o && stakeGenerationTarget o == Exactly ->
                                 -- Cover 'amountToCover' exactly, on the old accumulator.
                                 finalise genValue getValue amountToCover acc
                             | otherwise ->
                                 case amountToCover `Core.subCoin` coin of
                                     Nothing        -> error "invariant violated!"
                                     Just remaining -> go remaining acc'


-- | Generate an Utxo. The returned Utxo is not valid from a Cardano
-- perspective, but it is for coin selection.
genUtxo :: StakeGenOptions -> Gen Core.Utxo
genUtxo o = do
    let genValue mbAcc coins = do
          txIn <- (Core.TxInUtxo <$> arbitrary <*> arbitrary) `suchThat`
                  (\t -> case mbAcc of
                             Nothing   -> True
                             Just utxo -> not (Map.member t utxo)
                  )
          addr <- arbitraryAddress o
          let txOutAux = Core.TxOutAux (Core.TxOut addr coins)
          return $ Map.singleton txIn txOutAux
    fromStakeOptions o genValue utxoBalance

-- | Generate some Utxo with @at least@ the supplied amount of money.
genUtxoWithAtLeast :: InitialBalance -> Gen Core.Utxo
genUtxoWithAtLeast payment = do
    genUtxo $ StakeGenOptions {
                  stakeOnEachInputPercentage = Just 0.2
                , stakeGenerationTarget = AtLeast
                , stakeNeeded           = Core.mkCoin (toLovelaces payment)
                , stakeMaxEntries       = Just 100
                , fiddlyAddresses       = False
                , allowRedeemAddresses  = False
            }

{-------------------------------------------------------------------------------
  Dealing with grouping
-------------------------------------------------------------------------------}

genGroupedUtxo :: InitialBalance -> Gen Core.Utxo
genGroupedUtxo balance = do
    let toHave = case balance of
                     InitialLovelace amount -> amount
                     InitialADA amount      -> amount * 1000000

    -- This is the 'sink address' all of the Utxo entries have "paid into".
    sinkAddress <- arbitrary `suchThat` (not . Core.isRedeemAddress)

    -- Generate entries such that the unspent output has all the same address,
    -- i.e. the 'sinkAddress'.
    -- NOTE: We are really generating a pathological input as, in practice, we
    -- will have more than one group.
    entries <- forM (divideInto 10 toHave) $ \slice -> do
                   txIn <- Core.TxInUtxo <$> arbitrary <*> arbitrary
                   let txOutAux = Core.TxOutAux (Core.TxOut sinkAddress (Core.mkCoin slice))
                   return (txIn, txOutAux)
    return (Map.fromList entries)

-- | Divides the input 'Word64' into 'Int' different \"buckets\" all having
-- the same value. If the initial quantity is not equally divideable, the last
-- bucket will have the greatest value.
divideInto :: Int
           -- ^ Into how many parts divide
           -> Word64
           -- ^ The input quantity
           -> [Word64]
           -- ^ A list of \"buckets\" all having the same size, if possible.
divideInto 0 _   = error "divideInto called on 0"
divideInto n qty = go n (quotRem qty (fromIntegral n)) mempty
    where
        go 0 (_, _) !acc             = acc
        go 1 (sliceSize, slack) !acc = (sliceSize + slack) : acc
        go x (sliceSize, slack) !acc = go (x - 1) (sliceSize, slack) (sliceSize : acc)

-- | Generates a \"unique\" change address. A change address is unique is this
-- context if is not part of the Utxo & is not one of the original outputs
-- we need to pay.
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

genTxOut :: StakeGenOptions -> Gen (NonEmpty Core.TxOut)
genTxOut opts = fromStakeOptions opts genOne paymentAmount
    where
        genOne :: Maybe (NonEmpty Core.TxOut) -> Core.Coin -> Gen (NonEmpty Core.TxOut)
        genOne _ coins = do
            addr  <- arbitraryAddress opts
            return (Core.TxOut addr coins :| [])

-- | Generates multiple payees.
genPayees :: Pay -> Gen (NonEmpty Core.TxOut)
genPayees payment = do
    genTxOut StakeGenOptions {
               stakeOnEachInputPercentage = Just 0.15
             , stakeGenerationTarget = AtLeast
             , stakeNeeded           = Core.mkCoin (toLovelaces payment)
             , stakeMaxEntries       = Just 5
             , fiddlyAddresses       = False
             , allowRedeemAddresses  = False
             }

-- | Generates a single payee.
genPayee :: Pay -> Gen (NonEmpty Core.TxOut)
genPayee payment = do
    genTxOut StakeGenOptions {
              stakeOnEachInputPercentage = Nothing
            , stakeGenerationTarget = AtLeast
            , stakeNeeded           = Core.mkCoin (toLovelaces payment)
            , stakeMaxEntries       = Just 1
            , fiddlyAddresses       = False
            , allowRedeemAddresses  = False
            }

-- | Generates a single payee which has a redeem address inside.
genRedeemPayee :: Pay -> Gen (NonEmpty Core.TxOut)
genRedeemPayee payment = do
    a <- arbitrary `suchThat` Core.isRedeemAddress
    return (Core.TxOut a (Core.mkCoin (toLovelaces payment)) :| [])
