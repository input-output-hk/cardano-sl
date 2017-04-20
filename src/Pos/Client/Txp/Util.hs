-- | Pure functions for operations with transactions

module Pos.Client.Txp.Util
       (
       -- * Tx creation
         makePubKeyTx
       , makeMOfNTx
       , makeRedemptionTx
       , createTx
       , createMTx
       , createMOfNTx
       , createRedemptionTx

       -- * Additional datatypes
       , TxError
       ) where

import           Control.Lens        ((%=), (.=))
import           Control.Monad.State (StateT (..), evalStateT)
import qualified Data.HashMap.Strict as HM
import           Data.List           (tail)
import           Data.List.NonEmpty  ((<|))
import qualified Data.Map            as M
import qualified Data.Vector         as V
import           Universum

import           Pos.Binary          ()
import           Pos.Core.Coin       (unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Crypto          (PublicKey, RedeemSecretKey, SafeSigner,
                                      SignTag (SignTxIn), hash, redeemSign,
                                      redeemToPublic, safeSign, safeToPublic)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Script          (Script)
import           Pos.Script.Examples (multisigRedeemer, multisigValidator)
import           Pos.Txp             (Tx (..), TxAux, TxDistribution (..), TxIn (..),
                                      TxInWitness (..), TxOut (..), TxOutAux (..),
                                      TxSigData (..), Utxo, filterUtxoByAddrs)
import           Pos.Types           (Address, Coin, makePubKeyAddress, makeRedeemAddress,
                                      makeScriptAddress, mkCoin, sumCoins)

type TxInputs = NonEmpty (Address, TxIn)
type TxOutputs = NonEmpty TxOutAux
type TxError = Text

-----------------------------------------------------------------------------
-- Tx creation
-----------------------------------------------------------------------------

-- | Generic function to create a transaction, given desired inputs, outputs and a
-- way to construct witness from signature data
makeAbstractTx :: (Address -> TxSigData -> TxInWitness) -> TxInputs -> TxOutputs -> TxAux
makeAbstractTx mkWit txInputs outputs = ( UnsafeTx (map snd txInputs) txOutputs txAttributes
                                        , txWitness
                                        , txDist)
  where
    txOutputs = map toaOut outputs
    txAttributes = mkAttributes ()
    txOutHash = hash txOutputs
    txDist = TxDistribution (map toaDistr outputs)
    txDistHash = hash txDist
    txWitness = V.fromList $ toList $ txInputs <&>
        \(addr, txIn) -> mkWit addr $ makeTxSigData txIn
    makeTxSigData txIn =
        TxSigData { txSigInput     = txIn
                  , txSigOutsHash  = txOutHash
                  , txSigDistrHash = txDistHash
                  }

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx :: (Address -> SafeSigner) -> TxInputs -> TxOutputs -> TxAux
makePubKeyTx getSs = makeAbstractTx mkWit
  where mkWit addr sigData =
          let ss = getSs addr
          in PkWitness
              { twKey = safeToPublic ss
              , twSig = safeSign SignTxIn ss sigData
              }

makeMOfNTx :: Script -> [Maybe SafeSigner] -> TxInputs -> TxOutputs -> TxAux
makeMOfNTx validator sks = makeAbstractTx mkWit
  where mkWit _ sigData = ScriptWitness
            { twValidator = validator
            , twRedeemer = multisigRedeemer sigData sks
            }

makeRedemptionTx :: RedeemSecretKey -> TxInputs -> TxOutputs -> TxAux
makeRedemptionTx rsk = makeAbstractTx mkWit
  where rpk = redeemToPublic rsk
        mkWit _ sigData = RedeemWitness
            { twRedeemKey = rpk
            , twRedeemSig = redeemSign rsk sigData
            }

type FlatUtxo = [(TxIn, TxOutAux)]
type InputPicker = StateT (Coin, FlatUtxo) (Either TxError)

-- | Given Utxo, desired source addresses and desired outputs, prepare lists
-- of correct inputs and outputs to form a transaction
prepareInpsOuts :: Utxo -> NonEmpty Address -> TxOutputs -> Either TxError (TxInputs, TxOutputs)
prepareInpsOuts utxo addrs@(someAddr :| _) outputs = do
    futxo <- evalStateT (pickInputs []) (totalMoney, sortedUnspent)
    let inputSum =
            unsafeIntegerToCoin $ sumCoins $ map (txOutValue . toaOut . snd) futxo
        newOuts
            | inputSum > totalMoney =
                TxOutAux (TxOut someAddr (inputSum `unsafeSubCoin` totalMoney)) [] <|
                outputs
            | otherwise = outputs
    case nonEmpty futxo of
        Nothing       -> fail "Failed to prepare inputs!"
        Just inputsNE -> pure (map mkTxInput inputsNE, newOuts)
  where
    totalMoney = unsafeIntegerToCoin $ sumCoins $ map (txOutValue . toaOut) outputs
    allUnspent = M.toList $ filterUtxoByAddrs (toList addrs) utxo
    sortedUnspent =
        sortOn (Down . txOutValue . toaOut . snd) allUnspent
    pickInputs :: FlatUtxo -> InputPicker FlatUtxo
    pickInputs inps = do
        moneyLeft <- use _1
        if moneyLeft == mkCoin 0
            then return inps
            else do
                mNextOut <- head <$> use _2
                case mNextOut of
                    Nothing -> fail "Not enough money to send!"
                    Just inp@(_, (TxOutAux (TxOut {..}) _)) -> do
                        _1 .= unsafeSubCoin moneyLeft (min txOutValue moneyLeft)
                        _2 %= tail
                        pickInputs (inp : inps)
    -- TODO rename
    mkTxInput (inp, TxOutAux TxOut {..} _) = (txOutAddress, inp)

-- | Common use case of 'prepaseInpsOuts' - with single source address
prepareInpOuts :: Utxo -> Address -> TxOutputs -> Either TxError (TxInputs, TxOutputs)
prepareInpOuts utxo addr outputs = prepareInpsOuts utxo (one addr) outputs

-- | Make a multi-transaction using given secret key and info for outputs
createMTx :: Utxo -> NonEmpty SafeSigner -> TxOutputs -> Either TxError TxAux
createMTx utxo sss outputs =
    uncurry (makePubKeyTx getSigner) <$>
    prepareInpsOuts utxo (makePubKeyAddress . safeToPublic <$> sss) outputs
  where
    signers =
        HM.fromList
            [ (makePubKeyAddress $ safeToPublic ss, ss)
            | ss <- toList sss
            ]
    getSigner addr =
        fromMaybe (error "Requested signer for unknown address") $
        HM.lookup addr signers

-- | Make a multi-transaction using given secret key and info for outputs
createTx :: Utxo -> SafeSigner -> TxOutputs -> Either TxError TxAux
createTx utxo ss outputs =
    uncurry (makePubKeyTx $ const ss) <$>
    prepareInpOuts utxo (makePubKeyAddress $ safeToPublic ss) outputs

-- | Make a transaction, using M-of-N script as a source
createMOfNTx :: Utxo -> [(PublicKey, Maybe SafeSigner)] -> TxOutputs -> Either TxError TxAux
createMOfNTx utxo keys outputs = uncurry (makeMOfNTx validator sks) <$> inpOuts
  where pks = map fst keys
        sks = map snd keys
        m = length $ filter isJust sks
        validator = multisigValidator m pks
        addr = makeScriptAddress validator
        inpOuts = prepareInpOuts utxo addr outputs

createRedemptionTx :: Utxo -> RedeemSecretKey -> TxOutputs -> Either TxError TxAux
createRedemptionTx utxo rsk outputs =
    uncurry (makeRedemptionTx rsk) <$>
    prepareInpOuts utxo (makeRedeemAddress $ redeemToPublic rsk) outputs
