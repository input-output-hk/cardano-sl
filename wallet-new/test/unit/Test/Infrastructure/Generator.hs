module Test.Infrastructure.Generator (
    -- * Generator model and corresponding generators
    GeneratorModel(..)
  , genChainUsingModel
  , genInductiveUsingModel
    -- * Specific models
    -- ** Simple model
  , simpleModel
    -- ** Cardano
  , cardanoModel
  , estimateCardanoFee
  , estimateSize
  ) where

import           Universum

import qualified Data.Set as Set
import           Test.QuickCheck

import           UTxO.Context
import           UTxO.DSL
import           UTxO.Generator
import           Wallet.Inductive
import           Wallet.Inductive.Generator

import           Pos.Core (TxSizeLinear, calculateTxSizeLinear)
import           Pos.Crypto (RequiresNetworkMagic (..))
import           Serokell.Data.Memory.Units (Byte, fromBytes)

{-------------------------------------------------------------------------------
  Generator model
-------------------------------------------------------------------------------}

-- | 'Chain' and 'Inductive' generator model
--
--  The generators are polymorphic in the types of addresses we have, and need
--  various parameters. Here we introduce a simple model from which we can
--  derive all of these arguments. See 'simpleModel' and 'cardanoModel'.
data GeneratorModel h a = GeneratorModel {
      -- | Bootstrap transaction
      gmBoot          :: Transaction h a

      -- | Addresses to work with
      --
      -- These will be the addresses we can transfers funds from and to
    , gmAllAddresses  :: [a]

     -- | Which subset of 'gmAllAddresses' can we choose from for @ours@?
    , gmPotentialOurs :: a -> Bool

      -- | Maximum number of addresses to use for @ours@
    , gmMaxNumOurs    :: Int

      -- | Estimate fees
    , gmEstimateFee   :: RequiresNetworkMagic -> Int -> [Value] -> Value
    }

genChainUsingModel :: (Hash h a, Ord a)
                   => RequiresNetworkMagic -> GeneratorModel h a -> Gen (Chain h a)
genChainUsingModel rnm GeneratorModel{..} =
    evalStateT (genChain params) initState
  where
    params    = defChainParams (gmEstimateFee rnm) gmAllAddresses
    initUtxo  = utxoRestrictToAddr (`elem` gmAllAddresses) $ trUtxo gmBoot
    initState = initTrState initUtxo 1

genInductiveUsingModel :: (Hash h a, Ord a) => RequiresNetworkMagic
                       -> GeneratorModel h a -> Gen (Inductive h a)
genInductiveUsingModel rnm GeneratorModel{..} = do
    numOurs <- choose (1, min (length potentialOurs) gmMaxNumOurs)
    addrs'  <- shuffle potentialOurs
    let ours = Set.fromList (take numOurs addrs')
    events  <- evalStateT (genWalletEvents (params ours)) initState
    return Inductive {
        inductiveBoot   = gmBoot
      , inductiveOurs   = ours
      , inductiveEvents = events
      }
  where
    potentialOurs = filter gmPotentialOurs gmAllAddresses
    params ours   =
        defEventsParams (gmEstimateFee rnm) gmAllAddresses ours initUtxo
    initUtxo      = utxoRestrictToAddr (`elem` gmAllAddresses) $ trUtxo gmBoot
    initState     = initEventsGlobalState 1

{-------------------------------------------------------------------------------
  Simple model
-------------------------------------------------------------------------------}

-- | Simplified generator model
--
-- Small values, simple addresses, and no fees
simpleModel :: GeneratorModel GivenHash Char
simpleModel = GeneratorModel {
      gmAllAddresses  = addrs
    , gmPotentialOurs = \_ -> True
    , gmEstimateFee   = \_ _ _ -> 0
    , gmMaxNumOurs    = 3
    , gmBoot          = Transaction {
                            trFresh = fromIntegral (length addrs) * initBal
                          , trIns   = Set.empty
                          , trOuts  = [Output a initBal | a <- addrs]
                          , trFee   = 0
                          , trHash  = 0
                          , trExtra = ["Simple bootstrap"]
                          }
    }
  where
    addrs :: [Char]
    addrs = ['a' .. 'g']

    initBal :: Value
    initBal = 10000

{-------------------------------------------------------------------------------
  Cardano model
-------------------------------------------------------------------------------}

-- | The Cardano itself (given the bootstrap transaction).
--
-- This is a model that results in something that we can translate to Cardano,
-- but since it deals with the " real world " it has all kinds of different
-- actors, large values, etc., and so is a bit difficult to debug when
-- looking at values manually.
cardanoModel :: TxSizeLinear
             -> Transaction GivenHash Addr -> GeneratorModel GivenHash Addr
cardanoModel linearFeePolicy boot = GeneratorModel {
      gmBoot          = boot
    , gmAllAddresses  = filter (not . isAvvmAddr) $ addrsInBoot boot
    , gmPotentialOurs = \_ -> True
    , gmEstimateFee   = estimateCardanoFee linearFeePolicy
    , gmMaxNumOurs    = 5
    }

{-| Estimate the size of a transaction, in bytes.

     The magic numbers appearing in the formula have the following origins:

       5 = 1 + 2 + 2, where 1 = tag for Tx type, and 2 each to delimit the
           TxIn and TxOut lists.

      42 = 2 + 1 + 34 + 5, where 2 = tag for TxIn ctor, 1 = tag for pair,
           34 = size of encoded Blake2b_256 Tx hash, 5 = max size of encoded
           CRC32 (range is 1..5 bytes, average size is just under 5 bytes).

      11 = 2 + 2 + 2 + 5, where the 2s are: tag for TxOut ctor, tag for Address
           ctor, and delimiters for encoded address. 5 = max size of CRC32.

      32 = 1 + 30 + 1, where the first 1 is a tag for a tuple length, the
           second 1 is the encoded address type. 30 = size of Blake2b_224
           hash of Address'.
-}
estimateSize :: Int      -- ^ Average size of @Attributes AddrAttributes@.
             -> Int      -- ^ Size of transaction's @Attributes ()@.
             -> Int      -- ^ Number of inputs to the transaction.
             -> [Value]  -- ^ Coin value of each output to the transaction.
             -> Byte     -- ^ Estimated size of the resulting transaction.
estimateSize saa sta ins outs
    = fromBytes . fromIntegral $
      5
    + 42 * ins
    + (11 + listSize (32 + (fromIntegral saa))) * length outs
    + sum (map intSize outs)
    + fromIntegral sta
  where
    intSize s =
        if | s <= 0x17       -> 1
           | s <= 0xff       -> 2
           | s <= 0xffff     -> 3
           | s <= 0xffffffff -> 5
           | otherwise       -> 9

    listSize s = s + intSize s

-- | Estimate the fee for a transaction that has @ins@ inputs
--   and @length outs@ outputs. The @outs@ lists holds the coin value
--   of each output.
--
--   NOTE: The average size of @Attributes AddrAttributes@ and
--         the transaction attributes @Attributes ()@ are both hard-coded
--         here with some (hopefully) realistic values.
estimateCardanoFee :: TxSizeLinear -> RequiresNetworkMagic -> Int -> [Value] -> Value
estimateCardanoFee linearFeePolicy rnm ins outs
    = round (calculateTxSizeLinear linearFeePolicy (estimateSize addrAttrSize 16 ins outs))
  where
    addrAttrSize = 128 + (case rnm of NMMustBeNothing -> 0
                                      NMMustBeJust    -> 4)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

addrsInBoot :: Transaction GivenHash a -> [a]
addrsInBoot = map outAddr . trOuts
