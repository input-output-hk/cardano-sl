-- | Pure functions for operations with transactions

module Pos.Client.Txp.Util
       (
       -- * Tx creation
         makePubKeyTx
       , makeMPubKeyTx
       , makeMOfNTx
       , makeRedemptionTx
       , createTx
       , createMTx
       , createMOfNTx
       , createRedemptionTx

       -- * Additional datatypes
       , TxError (..)
       ) where

import           Control.Lens        (traversed, (%=), (.=))
import           Control.Monad.State (StateT (..), evalStateT)
import qualified Data.HashMap.Strict as HM
import           Data.List           (tail)
import qualified Data.Map            as M
import qualified Data.Text.Buildable
import qualified Data.Vector         as V
import           Formatting          (bprint, stext, (%))
import           Universum

import           Pos.Binary          ()
import           Pos.Core.Address    (AddressIgnoringAttributes (AddressIA))
import           Pos.Core.Coin       (unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Crypto          (RedeemSecretKey, SafeSigner, SignTag (SignTx), hash,
                                      redeemSign, redeemToPublic, safeSign, safeToPublic)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Script          (Script)
import           Pos.Script.Examples (multisigRedeemer, multisigValidator)
import           Pos.Txp             (Tx (..), TxAux (..), TxDistribution (..), TxIn (..),
                                      TxInWitness (..), TxOut (..), TxOutAux (..),
                                      TxSigData (..), Utxo, filterUtxoByAddrs)
import           Pos.Types           (Address, Coin, StakeholderId, makePubKeyAddress,
                                      makePubKeyAddress, makeRedeemAddress,
                                      makeScriptAddress, mkCoin, sumCoins)

type TxInputs = NonEmpty TxIn
type TxOwnedInputs owner = NonEmpty (owner, TxIn)
type TxOutputs = NonEmpty TxOutAux

data TxError
    = TxError !Text
    deriving (Show, Generic)

instance Exception TxError

instance Buildable TxError where
    build (TxError msg) = bprint ("Transaction creation error ("%stext%")") msg

-----------------------------------------------------------------------------
-- Tx creation
-----------------------------------------------------------------------------

-- | Generic function to create a transaction, given desired inputs, outputs and a
-- way to construct witness from signature data
makeAbstractTx :: (owner -> TxSigData -> TxInWitness)
               -> TxOwnedInputs owner
               -> TxOutputs
               -> TxAux
makeAbstractTx mkWit txInputs outputs = TxAux tx txWitness txDistr
  where
    tx = UnsafeTx (map snd txInputs) txOutputs txAttributes
    txOutputs = map toaOut outputs
    txAttributes = mkAttributes ()
    txDistr = TxDistribution (map toaDistr outputs)
    txWitness = V.fromList $ toList $ txInputs <&>
        \(addr, _) -> mkWit addr txSigData
    txSigData = TxSigData
        { txSigTxHash = hash tx
        , txSigTxDistrHash = hash txDistr }

-- | Like `makePubKeyTx`, but allows usage of different signers
makeMPubKeyTx :: (owner -> SafeSigner)
              -> TxOwnedInputs owner
              -> TxOutputs
              -> TxAux
makeMPubKeyTx getSs = makeAbstractTx mkWit
  where mkWit addr sigData =
          let ss = getSs addr
          in PkWitness
              { twKey = safeToPublic ss
              , twSig = safeSign SignTx ss sigData
              }

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx :: SafeSigner -> TxInputs -> TxOutputs -> TxAux
makePubKeyTx ss txInputs =
    makeMPubKeyTx (const ss) (map ((), ) txInputs)

makeMOfNTx :: Script -> [Maybe SafeSigner] -> TxInputs -> TxOutputs -> TxAux
makeMOfNTx validator sks txInputs = makeAbstractTx mkWit (map ((), ) txInputs)
  where mkWit _ sigData = ScriptWitness
            { twValidator = validator
            , twRedeemer = multisigRedeemer sigData sks
            }

makeRedemptionTx :: RedeemSecretKey -> TxInputs -> TxOutputs -> TxAux
makeRedemptionTx rsk txInputs = makeAbstractTx mkWit (map ((), ) txInputs)
  where rpk = redeemToPublic rsk
        mkWit _ sigData = RedeemWitness
            { twRedeemKey = rpk
            , twRedeemSig = redeemSign rsk sigData
            }

type FlatUtxo = [(TxIn, TxOutAux)]
type InputPicker = StateT (Coin, FlatUtxo) (Either Text)

-- | Given Utxo, desired source addresses and desired outputs, prepare lists
-- of correct inputs and outputs to form a transaction
prepareInpsOuts
    :: Utxo
    -> NonEmpty Address
    -> TxOutputs
    -> Either Text (TxOwnedInputs Address, TxOutputs)
prepareInpsOuts utxo addrs outputs = do
    when (totalMoney == mkCoin 0) $
        fail "Attempted to send 0 money"
    futxo <- evalStateT (pickInputs []) (totalMoney, sortedUnspent)
    case nonEmpty futxo of
        Nothing       -> fail "Failed to prepare inputs!"
        Just inputsNE -> pure (map formTxInputs inputsNE, outputs)
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
    formTxInputs (inp, TxOutAux TxOut{..} _) = (txOutAddress, inp)

-- | Common use case of 'prepaseInpsOuts' - with single source address
prepareInpOuts :: Utxo -> Address -> TxOutputs -> Either Text (TxInputs, TxOutputs)
prepareInpOuts utxo addr outputs =
    prepareInpsOuts utxo (one addr) outputs <&>
    _1 . traversed %~ snd

-- | Make a multi-transaction using given secret key and info for outputs.
-- Currently used for HD wallets only, thus `HDAddressPayload` is required
createMTx :: Utxo -> NonEmpty (SafeSigner, Address) -> TxOutputs -> Either Text TxAux
createMTx utxo hwdSigners outputs =
    let addrs = map snd hwdSigners
    in  uncurry (makeMPubKeyTx getSigner) <$>
        prepareInpsOuts utxo addrs outputs
  where
    signers = HM.fromList . toList $ map (swap . second AddressIA) hwdSigners
    getSigner addr =
        fromMaybe (error "Requested signer for unknown address") $
        HM.lookup (AddressIA addr) signers

-- | Make a multi-transaction using given secret key and info for outputs
createTx :: Utxo -> SafeSigner -> TxOutputs -> Either Text TxAux
createTx utxo ss outputs =
    uncurry (makePubKeyTx ss) <$>
    prepareInpOuts utxo (makePubKeyAddress $ safeToPublic ss) outputs

-- | Make a transaction, using M-of-N script as a source
createMOfNTx :: Utxo
             -> [(StakeholderId, Maybe SafeSigner)]
             -> TxOutputs
             -> Either Text TxAux
createMOfNTx utxo keys outputs = uncurry (makeMOfNTx validator sks) <$> inpOuts
  where ids = map fst keys
        sks = map snd keys
        m = length $ filter isJust sks
        validator = multisigValidator m ids
        addr = makeScriptAddress validator
        inpOuts = prepareInpOuts utxo addr outputs

createRedemptionTx :: Utxo -> RedeemSecretKey -> TxOutputs -> Either Text TxAux
createRedemptionTx utxo rsk outputs =
    uncurry (makeRedemptionTx rsk) <$>
    prepareInpOuts utxo (makeRedeemAddress $ redeemToPublic rsk) outputs

