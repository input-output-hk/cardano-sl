-- | Pure functions for operations with transactions

module Pos.Wallet.Tx.Pure
       ( makePubKeyTx
       , makeMOfNTx
       , createTx
       , createMOfNTx
       , getRelatedTxs
       , deriveAddrHistory
       , deriveAddrHistoryPartial
       , TxError
       ) where

import           Control.Lens              ((%=))
import           Control.Monad.State       (StateT (..), evalStateT)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.DList                as DL
import           Data.List                 (tail)
import qualified Data.Map                  as M
import qualified Data.Vector               as V
import           Universum

import           Pos.Binary                ()
import           Pos.Crypto                (PublicKey, SecretKey, WithHash (..), hash,
                                            sign, toPublic, withHash)
import           Pos.Data.Attributes       (mkAttributes)
import           Pos.Script                (Script)
import           Pos.Script.Examples       (multisigRedeemer, multisigValidator)
import           Pos.Types                 (Address, Block, Coin, MonadUtxoRead (..),
                                            Tx (..), TxAux, TxDistribution (..), TxId,
                                            TxIn (..), TxInWitness (..), TxOut (..),
                                            TxOutAux, TxSigData, TxWitness, Utxo,
                                            UtxoStateT (..), applyTxToUtxo, blockTxas,
                                            filterUtxoByAddr, makePubKeyAddress,
                                            makeScriptAddress, mkCoin, sumCoins,
                                            topsortTxs)
import           Pos.Types.Coin            (unsafeIntegerToCoin, unsafeSubCoin)

type TxOutIdx = (TxId, Word32)
type TxInputs = [TxOutIdx]
type TxOutputs = [TxOutAux]
type TxError = Text

-----------------------------------------------------------------------------
-- Tx creation
-----------------------------------------------------------------------------

-- | Generic function to create a transaction, given desired inputs, outputs and a
-- way to construct witness from signature data
makeAbstractTx :: (TxSigData -> TxInWitness) -> TxInputs -> TxOutputs -> TxAux
makeAbstractTx mkWit inputs outputs = (Tx {..}, txWitness, txDist)
  where txInputs = map makeTxIn inputs
        txOutputs = map fst outputs
        txAttributes = mkAttributes ()
        txOutHash = hash txOutputs
        txDist = TxDistribution (map snd outputs)
        txDistHash = hash txDist
        txWitness = V.fromList $ map (mkWit . makeTxSigData) inputs
        makeTxIn (txInHash, txInIndex) = TxIn {..}
        makeTxSigData (txInHash, txInIndex) = (txInHash, txInIndex, txOutHash, txDistHash)

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx :: SecretKey -> TxInputs -> TxOutputs -> TxAux
makePubKeyTx sk = makeAbstractTx mkWit
  where pk = toPublic sk
        mkWit sigData = PkWitness
            { twKey = pk
            , twSig = sign sk sigData
            }

makeMOfNTx :: Script -> [Maybe SecretKey] -> TxInputs -> TxOutputs -> TxAux
makeMOfNTx validator sks = makeAbstractTx mkWit
  where mkWit sigData = ScriptWitness
            { twValidator = validator
            , twRedeemer = multisigRedeemer sigData sks
            }

type FlatUtxo = [(TxOutIdx, TxOutAux)]
type InputPicker = StateT (Coin, FlatUtxo) (Either TxError)

-- | Given Utxo, desired source address and desired outputs, prepare lists
-- of correct inputs and outputs to form a transaction
prepareInpOuts :: Utxo -> Address -> TxOutputs -> Either TxError (TxInputs, TxOutputs)
prepareInpOuts utxo addr outputs = do
    futxo <- evalStateT (pickInputs []) (totalMoney, sortedUnspent)
    let inputs = map fst futxo
        inputSum = unsafeIntegerToCoin $
                   sumCoins $ map (txOutValue . fst . snd) futxo
        newOuts
            | inputSum > totalMoney =
                  (TxOut addr (inputSum `unsafeSubCoin` totalMoney), [])
                  : outputs
            | otherwise = outputs
    pure (inputs, newOuts)
  where
    totalMoney = unsafeIntegerToCoin $
                 sumCoins $ map (txOutValue . fst) outputs
    allUnspent = M.toList $ filterUtxoByAddr addr utxo
    sortedUnspent = sortBy (comparing $ Down . txOutValue . fst . snd) allUnspent

    pickInputs :: FlatUtxo -> InputPicker FlatUtxo
    pickInputs inps = do
        moneyLeft <- use _1
        if moneyLeft == mkCoin 0
            then return inps
            else do
                mNextOut <- head <$> use _2
                case mNextOut of
                    Nothing -> fail "Not enough money to send!"
                    Just inp@(_, (TxOut{..}, _)) -> do
                        _1 %= unsafeSubCoin (min txOutValue moneyLeft)
                        _2 %= tail
                        pickInputs (inp : inps)


-- | Make a multi-transaction using given secret key and info for outputs
createTx :: Utxo -> SecretKey -> TxOutputs -> Either TxError TxAux
createTx utxo sk outputs =
    uncurry (makePubKeyTx sk) <$>
    prepareInpOuts utxo (makePubKeyAddress $ toPublic sk) outputs

-- | Make a transaction, using M-of-N script as a source
createMOfNTx :: Utxo -> [(PublicKey, Maybe SecretKey)] -> TxOutputs -> Either TxError TxAux
createMOfNTx utxo keys outputs = uncurry (makeMOfNTx validator sks) <$> inpOuts
  where pks = map fst keys
        sks = map snd keys
        m = length $ filter isJust sks
        validator = multisigValidator m pks
        addr = makeScriptAddress validator
        inpOuts = prepareInpOuts utxo addr outputs

----------------------------------------------------------------------
-- Deduction of history
----------------------------------------------------------------------

-- | Check if given 'Address' is one of the receivers of 'Tx'
hasReceiver :: Tx -> Address -> Bool
hasReceiver Tx {..} addr = any ((== addr) . txOutAddress) txOutputs

-- | Given some 'Utxo', check if given 'Address' is one of the senders of 'Tx'
hasSender :: MonadUtxoRead m => Tx -> Address -> m Bool
hasSender Tx {..} addr = anyM hasCorrespondingOutput txInputs
  where hasCorrespondingOutput txIn =
            fmap toBool $ fmap ((== addr) . txOutAddress . fst) <$> utxoGet txIn
        toBool Nothing  = False
        toBool (Just b) = b

type TxSelectorT m = UtxoStateT (MaybeT m)

-- | Select transactions related to given address. `Bool` indicates
-- whether the transaction is outgoing (i. e. is sent from given address)
getRelatedTxs
    :: Monad m
    => Address
    -> [(WithHash Tx, TxWitness, TxDistribution)]
    -> TxSelectorT m [(TxId, Tx, Bool)]
getRelatedTxs addr txs = fmap DL.toList $
    lift (MaybeT $ return $ topsortTxs (view _1) txs) >>=
    foldlM step DL.empty
  where
    step ls (WithHash tx txId, _wit, dist) = do
        let isIncoming = tx `hasReceiver` addr
        isOutgoing <- tx `hasSender` addr
        if isOutgoing || isIncoming
            then do
            applyTxToUtxo (WithHash tx txId) dist
            identity %= filterUtxoByAddr addr
            return $ ls <> DL.singleton (txId, tx, isOutgoing)
            else return ls

-- | Given a full blockchain, derive address history and Utxo
-- TODO: Such functionality will still be useful for merging
-- blockchains when wallet state is ready, but some metadata for
-- Tx will be required.
deriveAddrHistory :: Monad m => Address -> [Block ssc] -> TxSelectorT m [(TxId, Tx, Bool)]
deriveAddrHistory addr chain = identity %= filterUtxoByAddr addr >>
                               deriveAddrHistoryPartial [] addr chain

deriveAddrHistoryPartial
    :: Monad m
    => [(TxId, Tx, Bool)]
    -> Address
    -> [Block ssc]
    -> TxSelectorT m [(TxId, Tx, Bool)]
deriveAddrHistoryPartial hist addr chain =
    DL.toList <$> foldrM updateAll (DL.fromList hist) chain
  where
    updateAll (Left _) hst = pure hst
    updateAll (Right blk) hst = do
        txs <- getRelatedTxs addr $
                   map (over _1 withHash) (blk ^. blockTxas)
        return $ DL.fromList txs <> hst
