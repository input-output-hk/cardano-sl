{-# LANGUAGE ViewPatterns #-}
module Test.Spec.CoinSelection.Generators (
      genGroupedUtxo
    , genPayee
    , genPayees
    , genFiddlyPayees
    , genUtxo
    , genFiddlyUtxo
    , StakeGenOptions(..)
    , GenerationTarget(..)
    , toLovelaces
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

import qualified Pos.Chain.Txp as Core
import           Pos.Core ()
import qualified Pos.Core as Core
import           Pos.Crypto ()

import           Util.Buildable ()

import           Cardano.Wallet.Kernel.CoinSelection ()
import           Cardano.Wallet.Kernel.Util.Core (paymentAmount, utxoBalance)

import           Test.Pos.Core.Arbitrary ()

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
      stakeMaxValue         :: Maybe Core.Coin
    -- ^ Never generate an entry with more than 'Core.Coin' on it.
    , stakeGenerationTarget :: GenerationTarget
    -- ^ How close we want to hit our target.
    , stakeNeeded           :: Core.Coin
    -- ^ How much stake we want to generate
    , stakeMaxEntries       :: Maybe Int
    -- ^ If specified, stop generating entries if they exceed the supplied
    -- number, to keep the Utxo size under control.
    , fiddlyAddresses       :: Bool
    -- ^ If set to 'True', the generator will try to produce addresses which
    -- base58 encoding is < 104 characters, as in the past this created problems
    -- to the coin selection algorithm.
    , allowRedeemAddresses  :: Bool
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
    let fiddlyCondition a = not (fiddlyAddresses opts) ||
                                (length (sformat F.build a) < 104)
    let redeemCondition a = allowRedeemAddresses opts ||
                            not (Core.isRedeemAddress a)
    arbitrary `suchThat` (\a -> fiddlyCondition a && redeemCondition a)


-- | Finalise the generation of 'a' by transferring all the remaining \"slack\".
finalise :: Semigroup a
         => (Maybe a -> Core.Coin -> Gen a)
         -- ^ A function to generate a value of type 'a', together with
         -- the currently accumulated value, if any.
         -> Core.Coin
         -- ^ The amount to cover
         -> a
         -- ^ The current accumulator
         -> Gen a
finalise genValue remaining acc = do
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
       else do c <- genCoin
               genValue Nothing c >>= go (stakeNeeded o `Core.unsafeSubCoin` c)
    where
        genCoin :: Gen Core.Coin
        genCoin = do
            money <- case stakeMaxValue o of
                         Nothing -> choose (1, Core.getCoin (stakeNeeded o))
                         Just r  -> choose (1, min (Core.getCoin r) (Core.getCoin $ stakeNeeded o))
            return $ Core.mkCoin money

        needToStop :: Int -> Maybe Int -> Bool
        needToStop _ Nothing               = False
        needToStop actual (Just requested) = (actual + 1) == requested

        go :: Core.Coin -> a -> Gen a
        go amountToCover acc =
            case needToStop (length acc) (stakeMaxEntries o) of
                 True  -> finalise genValue amountToCover acc
                 False -> do
                     coin <- genCoin
                     acc' <- ((<>) acc) <$> genValue (Just acc) coin
                     case getValue acc' of
                         bal | bal >= stakeNeeded o && stakeGenerationTarget o == AtLeast -> return acc'
                         bal | bal >= stakeNeeded o && stakeGenerationTarget o == Exactly ->
                                 -- Cover 'amountToCover' exactly, on the old accumulator.
                                 finalise genValue amountToCover acc
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
genFiddlyUtxo :: InitialBalance -> Gen Core.Utxo
genFiddlyUtxo payment = do
    let balance         = toLovelaces payment
        twentyPercentOf = balance `div` 5
    genUtxo $ StakeGenOptions {
                  stakeMaxValue         = Just (Core.mkCoin twentyPercentOf)
                , stakeGenerationTarget = Exactly
                , stakeNeeded           = Core.mkCoin balance
                , stakeMaxEntries       = Just 200
                , fiddlyAddresses       = True
                , allowRedeemAddresses  = False
            }

-- | Generate some Utxo with @at least@ the supplied amount of money.
genUtxoWithAtLeast :: InitialBalance -> Gen Core.Utxo
genUtxoWithAtLeast payment = do
    let balance         = toLovelaces payment
        twentyPercentOf = balance `div` 5
    genUtxo $ StakeGenOptions {
                  stakeMaxValue       = Just (Core.mkCoin twentyPercentOf)
                , stakeGenerationTarget = AtLeast
                , stakeNeeded           = Core.mkCoin (toLovelaces payment)
                , stakeMaxEntries       = Just 100
                , fiddlyAddresses       = False
                , allowRedeemAddresses  = False
            }

{-------------------------------------------------------------------------------
  Dealing with grouping
-------------------------------------------------------------------------------}

genGroupedUtxo :: Int
               -- ^ The number of groups to generate
               -> InitialBalance
               -> Gen Core.Utxo
genGroupedUtxo groups balance = do
    let toHave = toLovelaces balance

    -- This is the 'sink address' all of the Utxo entries have "paid into".
    sinkAddress <- arbitrary `suchThat` (not . Core.isRedeemAddress)

    -- Generate entries such that the unspent output has all the same address,
    -- i.e. the 'sinkAddress'.
    -- NOTE: We are really generating a pathological input as, in practice, we
    -- will have more than one group.
    entries <- forM (divideInto groups toHave) $ \groupSlice ->
                   forM (divideInto 10 groupSlice) $ \slice -> do
                       txIn <- Core.TxInUtxo <$> arbitrary <*> arbitrary
                       let txOutAux = Core.TxOutAux (Core.TxOut sinkAddress (Core.mkCoin slice))
                       return (txIn, txOutAux)
    return (Map.fromList . mconcat $ entries)

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

utxoSmallestEntry :: Core.Utxo -> Core.Coin
utxoSmallestEntry utxo =
    case sort (Map.toList utxo) of
         [] -> error "utxoSmallestEntry: invariant violated, empty Utxo given."
         (x:_) -> Core.txOutValue . Core.toaOut $ snd x

-- | Generates multiple payees.
genPayees :: Core.Utxo -> Pay -> Gen (NonEmpty Core.TxOut)
genPayees utxo payment = do
    let balance            = toLovelaces payment
        halfOfUtxoSmallest = (Core.getCoin $ utxoSmallestEntry utxo) `div` 2
    genTxOut StakeGenOptions {
               stakeMaxValue         = Just (Core.mkCoin halfOfUtxoSmallest)
             , stakeGenerationTarget = AtLeast
             , stakeNeeded           = Core.mkCoin balance
             -- NOTE: A sufficient (but not necessary) condition for random
             -- input selection to not fail is:
             --
             -- 1. We have twice as many outputs in the UTxO as payments
             -- 2. The smallest UTxO entry is at least twice the largest payment.
             --
             -- Then when random selection considers a single output, it will
             -- use at most 2 outputs (since any more would exceed the upper
             -- bound on the size of the change, set to 1x the payment value),
             -- and hence leave sufficient UTxO entries to cover the remaining
             -- payments.
             -- In principle it would suffice to have the (smallest) pair of
             -- UTxO entries be 3x the size of the (largest) payment; setting
             -- this to 4x means we can also cover the fees from the change
             -- outputs, without needing additional UTxO entries.
             , stakeMaxEntries       = Just $ length utxo `div` 2
             , fiddlyAddresses       = False
             , allowRedeemAddresses  = False
             }

-- | Generate \"fiddly\" payees, which are payees which might include addresses
-- which length is < 104 chars.
genFiddlyPayees :: Core.Utxo -> Pay -> Gen (NonEmpty Core.TxOut)
genFiddlyPayees utxo payment = do
    let balance            = toLovelaces payment
        halfOfUtxoSmallest = (Core.getCoin $ utxoSmallestEntry utxo) `div` 2
    genTxOut $ StakeGenOptions {
                  stakeMaxValue         = Just (Core.mkCoin halfOfUtxoSmallest)
                , stakeGenerationTarget = Exactly
                , stakeNeeded           = Core.mkCoin balance
                , stakeMaxEntries       = Just $ length utxo `div` 2
                , fiddlyAddresses       = True
                , allowRedeemAddresses  = False
            }

-- | Generates a single payee.
genPayee :: Core.Utxo -> Pay -> Gen (NonEmpty Core.TxOut)
genPayee _utxo payment = do
    let balance            = toLovelaces payment
    genTxOut StakeGenOptions {
              stakeMaxValue         = Nothing
            , stakeGenerationTarget = AtLeast
            , stakeNeeded           = Core.mkCoin balance
            , stakeMaxEntries       = Just 1
            , fiddlyAddresses       = False
            , allowRedeemAddresses  = False
            }

-- | Generates a single payee which has a redeem address inside.
genRedeemPayee :: Core.Utxo -> Pay -> Gen (NonEmpty Core.TxOut)
genRedeemPayee _utxo payment = do
    a <- arbitrary `suchThat` Core.isRedeemAddress
    return (Core.TxOut a (Core.mkCoin (toLovelaces payment)) :| [])
