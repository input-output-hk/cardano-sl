{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

module Bench.Pos.Criterion.TxVerifyingBench
       ( runBenchmark
       ) where

import           Control.Lens         (view, _1, _3, _4)
import           Criterion.Main       (Benchmark, bench, bgroup, defaultConfig,
                                       defaultMainWith, env, nf)
import           Criterion.Types      (Config (..))
import           Data.DeriveTH        (derive, makeNFData)
import           Data.List            (zipWith3)
import           Data.Maybe
import           Data.Text.Buildable  (Buildable)
import qualified Data.Text.Buildable  as Buildable
import qualified Data.Vector          as V (Vector, fromList)
import           Formatting           (bprint, build, int, sformat, (%))
import           Serokell.Util.Verify (VerificationRes, isVerSuccess, verifyGeneric)
import           Test.QuickCheck      (Arbitrary, Gen, NonEmptyList (..), arbitrary,
                                       generate)
import           Universum

import           Pos.Binary           (Bi)
import           Pos.Crypto           (PublicKey, SecretKey, Signature, checkSig, hash,
                                       shortHashF, sign, toPublic, withHash)
import           Pos.Script           (Script, txScriptCheck)
import           Pos.Types            (Coin, GoodTx (..), Tx (..), TxId, TxIn (..),
                                       TxOut (..), TxWitness, Utxo, applyTxToUtxoPure,
                                       checkPubKeyAddress, checkScriptAddress, findTxIn,
                                       makePubKeyAddress, verifyTxAlone, verifyTxUtxoPure)

newVerifyTx :: (Utxo, Tx, TxWitness) -> Bool
newVerifyTx (u, tx, tw) = isVerSuccess $ verifyTxUtxoPure u (tx, tw)

oldVerifyTx :: (Utxo, Tx, OldTxWitness) -> Bool
oldVerifyTx (u, tx, tw) = isVerSuccess $ oldVerifyTxUtxo u (tx, tw)

genArgs :: forall a t. Arbitrary a => (a -> [(Tx, TxIn, TxOut, t)]) -> IO (Utxo, Tx, V.Vector t)
genArgs txGetter = generate $ do
    ls <- txGetter <$> arbitrary
    let txs = fmap (view _1) ls
        witness = V.fromList $ fmap (view _4) ls
        newTx = uncurry Tx $ unzip $ fmap (\(_, tIs, tOs, _) -> (tIs, tOs)) ls
        utxo = foldl' (flip applyTxToUtxoPure) mempty (fmap withHash txs)
    return (utxo, newTx, witness)

newTxVerifyBench :: Benchmark
newTxVerifyBench =
    env (genArgs getGoodTx) $ bench "Transaction verifying" . nf newVerifyTx

oldTxVerifyBench :: Benchmark
oldTxVerifyBench =
    env (genArgs getOldTx) $ bench "Old transaction verifying" . nf oldVerifyTx

txVerifyConfig :: Config
txVerifyConfig = defaultConfig
    { reportFile = Just "txVerifying.html"
    }

runBenchmark :: IO ()
runBenchmark = defaultMainWith txVerifyConfig $
    [ bgroup "Tx verification" [newTxVerifyBench, oldTxVerifyBench] ]

verifyTx
    :: (TxIn -> Maybe TxOut)
    -> (Tx, OldTxWitness)
    -> VerificationRes
verifyTx inputResolver (tx@Tx{..}, witnesses) =
    mconcat [verifyTxAlone tx, verifyCounts, verifySum, verifyInputs]
  where
    outSum :: Integer
    outSum = sum $ fmap (toInteger . txOutValue) txOutputs
    extendedInputs :: [Maybe (TxIn, TxOut)]
    extendedInputs = fmap extendInput txInputs
    extendInput txIn = (txIn,) <$> inputResolver txIn
    resolvedInputs = catMaybes extendedInputs
    inpSum :: Integer
    inpSum = sum $ fmap (toInteger . txOutValue . snd) resolvedInputs
    verifyCounts =
        verifyGeneric
            [ ( length txInputs == length witnesses
              , sformat ("length of inputs != length of witnesses "%
                         "("%int%" != "%int%")")
                  (length txInputs) (length witnesses) )
            ]
    verifySum =
        let resInps = length resolvedInputs
            extInps = length extendedInputs
            allInputsExist = resInps == extInps
            verifier =
                if allInputsExist
                    then ( inpSum >= outSum
                         , sformat
                               ("sum of outputs is more than sum of inputs ("
                                %int%" > "%int)
                                outSum inpSum)
                    else ( False
                         , sformat
                               (int%" inputs could not be resolved")
                               (abs $ resInps - extInps))
        in verifyGeneric [verifier]
    verifyInputs =
        verifyGeneric $ concat $
            zipWith3 inputPredicates [0..] extendedInputs (toList witnesses)

    inputPredicates
        :: Word                     -- ^ Input index
        -> Maybe (TxIn, TxOut)      -- ^ Input and corresponding output
        -> OldTxInWitness
        -> [(Bool, Text)]
    inputPredicates i Nothing _ =
        [(False, sformat ("input #"%int%" is not an unspent output") i)]
    inputPredicates i (Just (txIn@TxIn{..}, TxOut{..})) witness =
        [ ( checkAddrHash txOutAddress witness
          , sformat ("input #"%int%" doesn't match address "
                     %build%" of corresponding output: ("%build%")")
                i txOutAddress txIn
          )
        , ( validateTxIn txIn witness
          , sformat ("input #"%int%" isn't validated by its witness\n"%
                     "  input: "%build%"\n"%
                     "  witness: "%build)
                i txIn witness
          )
        ]

    checkAddrHash addr OldPkWitness{..}     = checkPubKeyAddress twKey addr
    checkAddrHash addr OldScriptWitness{..} = checkScriptAddress twValidator addr

    validateTxIn TxIn{..} OldPkWitness{..} =
        checkSig twKey (txInHash, txInIndex, txOutputs) twSig
    validateTxIn TxIn{..} OldScriptWitness{..} =
        isRight (txScriptCheck twValidator twRedeemer)

type OldTxSig = Signature (TxId, Word32, [TxOut])

data OldTxInWitness
    = OldPkWitness
        { twKey :: PublicKey
        , twSig :: OldTxSig
        }
    | OldScriptWitness
        { twValidator :: Script
        , twRedeemer  :: Script
        }
    deriving (Eq, Show, Generic)

type OldTxWitness = V.Vector OldTxInWitness

instance Bi Script => Buildable OldTxInWitness where
    build (OldPkWitness key sig) =
        bprint ("PkWitness: key = "%build%", sig = "%build) key sig
    build (OldScriptWitness val red) =
        bprint ("ScriptWitness: "%
                "validator hash = "%shortHashF%", "%
                "redeemer hash = "%shortHashF) (hash val) (hash red)

buildProperTx
    :: [(Tx, SecretKey, SecretKey, Coin)]
    -> (Coin -> Coin, Coin -> Coin)
    -> Gen [(Tx, TxIn, TxOut, OldTxInWitness)]
buildProperTx triplesList (inCoin, outCoin)= do
        let fun (Tx txIn txOut, fromSk, toSk, c) =
                let inC = inCoin c
                    outC = outCoin c
                    txToBeSpent = Tx txIn $ (makeTxOutput fromSk inC) : txOut
                in (txToBeSpent, fromSk, makeTxOutput toSk outC)
            txList = fmap fun triplesList
            thisTxOutputs = fmap (view _3) txList
            newTx (tx, fromSk, txOutput) =
                let txHash = hash tx
                    txIn = TxIn txHash 0
                    witness = OldPkWitness {
                        twKey = toPublic fromSk,
                        twSig = sign fromSk (txHash, 0, thisTxOutputs) }
                in (tx, txIn, txOutput, witness)
            makeTxOutput s c = TxOut (makePubKeyAddress $ toPublic s) c
            goodTx = fmap newTx txList
        return goodTx

newtype OldTx = OldTx
    { getOldTx :: [(Tx, TxIn, TxOut, OldTxInWitness)]
    } deriving (Show)

instance Bi Tx => Arbitrary OldTx where
    arbitrary = OldTx <$> do
        txsList <- getNonEmpty <$>
            (arbitrary :: Gen (NonEmptyList (Tx, SecretKey, SecretKey, Coin)))
        buildProperTx txsList (identity, identity)

oldVerifyTxUtxo :: Utxo -> (Tx, OldTxWitness) -> VerificationRes
oldVerifyTxUtxo utxo txw = verifyTx (`findTxIn` utxo) txw

derive makeNFData ''OldTxInWitness
