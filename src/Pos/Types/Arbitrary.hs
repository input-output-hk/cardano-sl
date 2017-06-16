{-# LANGUAGE TemplateHaskell #-}

-- | `Arbitrary` instances for core types for using in tests and benchmarks

module Pos.Types.Arbitrary
       ( BadSigsTx (..)
       , CoinPairOverflowSum (..)
       , CoinPairOverflowSub (..)
       , CoinPairOverflowMul (..)
       , DoubleInZeroToOneRange (..)
       , IntegerToCoinNoOverflow (..)
       , IntegerToCoinOverflow (..)
       , GoodTx (..)
       , goodTxToTxAux
       , LessThanZeroOrMoreThanOne (..)
       , SafeCoinPairMul (..)
       , SafeCoinPairSum (..)
       , SafeCoinPairSub (..)
       , SafeWord (..)
       , SmallBadSigsTx (..)
       , SmallHashMap (..)
       , SmallGoodTx (..)
       ) where

import           Universum

import qualified Data.ByteString           as BS (pack)
import           Data.Default              (def)
import           Data.DeriveTH             (derive, makeArbitrary)
import           Data.List.NonEmpty        ((<|))
import qualified Data.List.NonEmpty        as NE
import           Data.Time.Units           (Microsecond, Millisecond, fromMicroseconds)
import qualified Data.Vector               as V
import           Test.QuickCheck           (Arbitrary (..), Gen, choose, elements, oneof,
                                            scale, suchThat)
import           Test.QuickCheck.Instances ()

import           Pos.Binary.Class          (AsBinary, FixedSizeInt (..), Raw,
                                            SignedVarInt (..), UnsignedVarInt (..))
import           Pos.Binary.Core           ()
import           Pos.Binary.Crypto         ()
import           Pos.Binary.Txp            ()
import           Pos.Constants             (sharedSeedLength)
import           Pos.Core.Address          (makePubKeyAddress, makeRedeemAddress,
                                            makeScriptAddress)
import           Pos.Core.Coin             (coinToInteger, divCoin, unsafeSubCoin)
import           Pos.Core.Types            (BlockVersion (..), Coin, SoftwareVersion (..),
                                            applicationNameMaxLength, mkApplicationName)
import qualified Pos.Core.Types            as Types
import           Pos.Crypto                (Hash, PublicKey, SecretKey, Share,
                                            SignTag (SignTxIn), hash, sign, toPublic)
import           Pos.Crypto.Arbitrary      ()
import           Pos.Data.Attributes       (mkAttributes)
import           Pos.Merkle                (MerkleRoot (..), MerkleTree, mkMerkleTree)
import           Pos.Script                (Script)
import           Pos.Script.Examples       (badIntRedeemer, goodIntRedeemer, intValidator)
import           Pos.Txp.Core.Types        (Tx (..), TxAux (..), TxDistribution (..),
                                            TxIn (..), TxInWitness (..), TxOut (..),
                                            TxOutAux (..), TxProof (..), TxSigData (..),
                                            mkTx)
import           Pos.Types.Arbitrary.Core  ()
import           Pos.Util                  (makeSmall)

----------------------------------------------------------------------------
-- Arbitrary core types
----------------------------------------------------------------------------

instance Arbitrary Script where
    arbitrary = elements
        [intValidator, goodIntRedeemer, badIntRedeemer]

instance Arbitrary Types.Address where
    arbitrary = oneof [
        makePubKeyAddress <$> arbitrary,
        makeScriptAddress <$> arbitrary,
        makeRedeemAddress <$> arbitrary,
        Types.UnknownAddressType <$> choose (3, 255) <*> scale (min 150) arbitrary
        ]

deriving instance Arbitrary Types.ChainDifficulty

derive makeArbitrary ''TxOut
derive makeArbitrary ''TxOutAux
derive makeArbitrary ''TxSigData

instance Arbitrary Coin where
    arbitrary = Types.mkCoin <$> choose (1, Types.unsafeGetCoin maxBound)

-- | This datatype has two coins that will always overflow when added.
-- It is used in tests to make sure addition raises the appropriate exception when this
-- happens.
newtype CoinPairOverflowSum = TwoCoinsSum
    { get2CSum :: (Coin, Coin)
    } deriving (Show, Eq)

instance Arbitrary CoinPairOverflowSum where
    arbitrary = do
        c1 <- arbitrary
        let lowerBound = succ $ coinToInteger $ (maxBound @Coin) `unsafeSubCoin` c1
            upperBound = coinToInteger (maxBound @Coin)
        c2 <- Types.mkCoin . fromIntegral <$> choose (lowerBound, upperBound)
        return $ TwoCoinsSum (c1, c2)

-- | This datatype has two coins that will never overflow when added.
-- It is therefore safe to add them. Useful in tests to ensure adding two coins whose sum
-- is a valid 'Coin' always works.
newtype SafeCoinPairSum = CoinPairSum
    { getPairSum :: (Coin, Coin)
    } deriving (Show, Eq)

instance Arbitrary SafeCoinPairSum where
    arbitrary = do
        c1 <- arbitrary
        let upperBound = Types.unsafeGetCoin c1
            highestBound = Types.unsafeGetCoin maxBound
        c2 <- Types.mkCoin <$> choose (0, highestBound - upperBound)
        return $ CoinPairSum (c1, c2)

-- | This datatype has two coins that will always underflow when subtracted.
-- It is used in tests to make sure subtraction raises the appropriate exception when this
-- happens.
newtype CoinPairOverflowSub = TwoCoinsSub
    { get2CSub :: (Coin, Coin)
    } deriving (Show, Eq)

instance Arbitrary CoinPairOverflowSub where
    arbitrary = do
        firstCoin <- arbitrary
        let firstWord = Types.unsafeGetCoin firstCoin
            c1 = if firstCoin == maxBound
                then Types.mkCoin $ firstWord - 1
                else firstCoin
        c2 <- arbitrary `suchThat` (> c1)
        return $ TwoCoinsSub (c1, c2)

-- | This datatype has two coins that will never underflow when subtracted.
-- It is therefore safe to subtract them. Useful in tests to show that two coins whose
-- subtraction does not underflow always works.
newtype SafeCoinPairSub = CoinPairSub
    { getPairSub :: (Coin, Coin)
    } deriving (Show, Eq)

instance Arbitrary SafeCoinPairSub where
    arbitrary = do
        c1 <- arbitrary
        let upperBound = Types.unsafeGetCoin c1
        c2 <- Types.mkCoin <$> choose (0, upperBound)
        return $ CoinPairSub (c1, c2)

-- | This datatype has a 'Coin' and an 'Integer' that will always overflow when
-- multiplied.
-- It is used in tests to make sure multiplication raises the appropriate exception when
-- this happens.
newtype CoinPairOverflowMul = TwoCoinsM
    { get2CMul :: (Coin, Integer)
    } deriving (Show, Eq)

instance Arbitrary CoinPairOverflowMul where
    arbitrary = do
        c1 <- arbitrary
        let integralC1 = coinToInteger c1
            lowerBound =
                1 + (coinToInteger $ (maxBound @Coin) `divCoin` integralC1)
            upperBound = coinToInteger (maxBound @Coin)
        c2 <- fromIntegral @Integer <$> choose (lowerBound, upperBound)
        return $ TwoCoinsM (c1, c2)

-- | This datatype has a 'Coin' and an 'Integer'  that will always overflow when
-- multiplied.
-- It is used to make sure coin multiplication by an integer raises the appropriate
-- exception when this happens.
newtype SafeCoinPairMul = CoinPairMul
    { getPairMul :: (Coin, Integer)
    } deriving (Show, Eq)

instance Arbitrary SafeCoinPairMul where
    arbitrary = do
        c1 <- arbitrary
        let upperBound = coinToInteger c1
            highestBound = coinToInteger maxBound
        c2 <- choose (0, div highestBound upperBound)
        return $ CoinPairMul (c1, c2)

-- | 'IntegerToCoinOverflow' is a wrapped over 'Integer'. Its 'Arbitrary' instance makes
-- it so that these integers will always overflow when converted into a 'Coin'.
-- Used in tests to make sure an exception is raised when there is an attempt to turn an
-- excessively large 'Integer' into a 'Coin'.
newtype IntegerToCoinOverflow = LargeInteger
    { getLargeInteger :: Integer
    } deriving (Show, Eq)

instance Arbitrary IntegerToCoinOverflow where
    arbitrary = LargeInteger <$> do
        n <- succ . fromIntegral <$> (arbitrary :: Gen Word)
        let lowerBound = succ . coinToInteger $ maxBound @Coin
        num <- choose (lowerBound, n * lowerBound)
        return $ toInteger num

-- | This datatype has an Integer that will never overflow when turned into a 'Coin'.
-- Useful for testing that conversion between valid 'Integer's and 'Coin's works properly.
newtype IntegerToCoinNoOverflow = Integer
    { getInteger :: Integer
    } deriving (Show, Eq)

instance Arbitrary IntegerToCoinNoOverflow where
    arbitrary =
        Integer . fromIntegral <$> choose (0, Types.unsafeGetCoin $ maxBound @Coin)

instance Arbitrary Types.CoinPortion where
    arbitrary = Types.unsafeCoinPortionFromDouble . (1/) <$> choose (1, 20)

-- | A wrapper over 'Double'. Its 'Arbitrary' instance ensures the 'Double' within can
-- never be converted into a 'CoinPortion' without an exception being raised. Used in
-- tests to safeguard that converting an invalid 'Double' to a 'CoinPortion' always
-- raised an exception.
newtype LessThanZeroOrMoreThanOne = BadCoinPortion
    { getDouble :: Double
    } deriving (Show, Eq)

instance Arbitrary LessThanZeroOrMoreThanOne where
    arbitrary = BadCoinPortion <$> do
        d <- arbitrary
        return $ if (d >= 0 && d <= 1)
            then 10 / d
            else d

-- | Another wrapper over 'Double'. Its 'Arbitrary' instance guarantees the 'Double'
-- inside can always be safely turned into a 'CoinPortion'. Used in tests to ensure
-- converting a valid 'Double' to/from 'CoinPortion' works properly.
newtype DoubleInZeroToOneRange = DoubleInRange
    { getDoubleInRange :: Double
    } deriving (Show, Eq)

instance Arbitrary DoubleInZeroToOneRange where
    arbitrary = DoubleInRange <$> choose (0, 1)

-- | A wrapper over 'Word64'. Its 'Arbitrary' instance guarantees the 'Word64'
-- inside can always be safely converted into 'CoinPortion'. Used in tests to ensure
-- converting a valid 'Word64' to/from 'CoinPortion' works properly.
newtype SafeWord = SafeWord
    { getSafeWord :: Word64
    } deriving (Show, Eq)

instance Arbitrary SafeWord where
    arbitrary = SafeWord . Types.getCoinPortion <$> arbitrary

instance Arbitrary TxInWitness where
    arbitrary = oneof [
        PkWitness <$> arbitrary <*> arbitrary,
        -- this can generate a redeemer script where a validator script is
        -- needed and vice-versa, but it doesn't matter
        ScriptWitness <$> arbitrary <*> arbitrary,
        RedeemWitness <$> arbitrary <*> arbitrary,
        UnknownWitnessType <$> choose (3, 255) <*> scale (min 150) arbitrary ]

derive makeArbitrary ''TxDistribution
derive makeArbitrary ''TxIn

-- | Arbitrary transactions generated from this instance will only be valid
-- with regards to 'mxTx'
instance Arbitrary Tx where
    arbitrary =
        mkTx <$> arbitrary <*> arbitrary <*>
        pure (mkAttributes ()) <&> \case
            Left err -> error $ "Arbitrary Tx: " <> err
            Right res -> res

-- | Type used to generate valid ('verifyTx')
-- transactions and accompanying input information.
-- It's not entirely general because it only generates transactions whose
-- outputs are in the same number as its inputs in a one-to-one correspondence.
--
-- The GoodTx type is a nonempty list of quadruples. It contains
-- previous transaction input came from, the input itself, output and
-- witness for the input. Number of inputs is equal to number of outputs.
--
-- The OverflowTx type is the same as GoodTx, except its values, both for
-- inputs as well as outputs, are very close to maxBound :: Coin so as to cause
-- overflow in the Coin type if they are summed.
--
-- The BadSigTx type is also the same as GoodTx, with the difference that all
-- signatures in the transaction's inputs have been replaced with a bogus one.

buildProperTx
    :: NonEmpty (Tx, SecretKey, SecretKey, Coin)
    -> (Coin -> Coin, Coin -> Coin)
    -> NonEmpty ((Tx, TxDistribution), TxIn, TxOutAux, TxInWitness)
buildProperTx triplesList (inCoin, outCoin) = fmap newTx txList
  where
    fun (UnsafeTx txIn txOut _, fromSk, toSk, c) =
        let inC = inCoin c
            outC = outCoin c
            txToBeSpent =
                UnsafeTx
                    txIn
                    ((makeTxOutput fromSk inC) <| txOut)
                    (mkAttributes ())
        in (txToBeSpent, fromSk, makeTxOutput toSk outC)
    -- why is it called txList? I've no idea what's going on here (@neongreen)
    txList = fmap fun triplesList
    txOutsHash = hash $ fmap (view _3) txList
    distrHash = hash (TxDistribution (NE.fromList $ replicate (length txList) []))
    makeNullDistribution tx =
        TxDistribution (NE.fromList $ replicate (length (_txOutputs tx)) [])
    newTx (tx, fromSk, txOutput) =
        let txHash = hash tx
            txIn = TxIn txHash 0
            witness =
                PkWitness
                { twKey = toPublic fromSk
                , twSig = sign SignTxIn fromSk TxSigData{
                             txSigInput = txIn,
                             txSigOutsHash = txOutsHash,
                             txSigDistrHash = distrHash }
                }
        in ((tx, makeNullDistribution tx), txIn, (TxOutAux txOutput []), witness)
    makeTxOutput s c = TxOut (makePubKeyAddress $ toPublic s) c

-- | Well-formed transaction 'Tx'.
newtype GoodTx = GoodTx
    { getGoodTx :: NonEmpty ((Tx, TxDistribution), TxIn, TxOutAux, TxInWitness)
    } deriving (Show)

newtype SmallGoodTx = SmallGoodTx { getSmallGoodTx :: GoodTx } deriving (Show)

goodTxToTxAux :: GoodTx -> TxAux
goodTxToTxAux (GoodTx l) = TxAux tx witness distr
  where
    tx = fromMaybe (error "goodTxToTxAux created malformed tx") $
         mkTx (map (view _2) l) (map (toaOut . view _3) l) def
    witness = V.fromList $ NE.toList $ map (view _4) l
    distr = TxDistribution $ map (toaDistr . view _3) l

instance Arbitrary GoodTx where
    arbitrary =
        GoodTx <$> (buildProperTx <$> arbitrary <*> pure (identity, identity))

instance Arbitrary SmallGoodTx where
    arbitrary = SmallGoodTx <$> makeSmall arbitrary

-- | Ill-formed 'Tx' with bad signatures.
newtype BadSigsTx = BadSigsTx
    { getBadSigsTx :: NonEmpty ((Tx, TxDistribution), TxIn, TxOutAux, TxInWitness)
    } deriving (Show)

newtype SmallBadSigsTx =
    SmallBadSigsTx BadSigsTx
    deriving Show

instance Arbitrary BadSigsTx where
    arbitrary = BadSigsTx <$> do
        goodTxList <- getGoodTx <$> arbitrary
        badSig <- arbitrary
        return $ map (set _4 badSig) goodTxList

instance Arbitrary SmallBadSigsTx where
    arbitrary = SmallBadSigsTx <$> makeSmall arbitrary

instance Arbitrary (MerkleRoot Tx) where
    arbitrary = MerkleRoot <$> (arbitrary @(Hash Raw))

instance Arbitrary (MerkleTree Tx) where
    arbitrary = mkMerkleTree <$> arbitrary

instance Arbitrary TxProof where
    arbitrary = TxProof <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Types.SharedSeed where
    arbitrary = do
        bs <- replicateM sharedSeedLength (choose (0, 255))
        return $ Types.SharedSeed $ BS.pack bs

----------------------------------------------------------------------------
-- Arbitrary types from MainExtra[header/body]data
----------------------------------------------------------------------------

instance Arbitrary Types.ApplicationName where
    arbitrary =
        either (error . mappend "arbitrary @ApplicationName failed: ") identity .
        mkApplicationName .
        toText . map (chr . flip mod 128) . take applicationNameMaxLength <$>
        arbitrary

derive makeArbitrary ''Types.BlockVersion
derive makeArbitrary ''Types.SoftwareVersion

----------------------------------------------------------------------------
-- Arbitrary miscellaneous types
----------------------------------------------------------------------------

instance Arbitrary Millisecond where
    arbitrary = fromMicroseconds <$> choose (0, 600 * 1000 * 1000)

instance Arbitrary Microsecond where
    arbitrary = fromMicroseconds <$> choose (0, 600 * 1000 * 1000)

deriving instance Arbitrary Types.Timestamp

newtype SmallHashMap =
    SmallHashMap (HashMap PublicKey (HashMap PublicKey (AsBinary Share)))
    deriving Show

instance Arbitrary SmallHashMap where
    arbitrary = SmallHashMap <$> makeSmall arbitrary

derive makeArbitrary ''UnsignedVarInt
derive makeArbitrary ''SignedVarInt
derive makeArbitrary ''FixedSizeInt
