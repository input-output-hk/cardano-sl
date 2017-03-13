{-# LANGUAGE TemplateHaskell #-}

-- | Pure functions for operations with transactions

module Pos.Wallet.Tx.Pure
       (
       -- * Tx creation
         makePubKeyTx
       , makeMOfNTx
       , makeRedemptionTx
       , createTx
       , createMOfNTx
       , createRedemptionTx

       -- * History derivation
       , getRelatedTxs
       , deriveAddrHistory
       , deriveAddrHistoryPartial

       -- * Additional datatypes
       , TxHistoryEntry (..)
       , thTxId
       , thTx
       , thIsOutput
       , thDifficulty
       , TxError
       ) where

import           Control.Lens              (makeLenses, (%=))
import           Control.Monad.State       (StateT (..), evalStateT)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.DList                as DL
import           Data.List                 (tail)
import           Data.List.NonEmpty        (nonEmpty, (<|))
import qualified Data.Map                  as M
import qualified Data.Vector               as V
import           Universum

import           Pos.Binary                ()
import           Pos.Core.Coin             (unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Crypto                (PublicKey, RedeemSecretKey, SafeSigner,
                                            WithHash (..), hash, redeemSign,
                                            redeemToPublic, safeSign, safeToPublic,
                                            withHash)
import           Pos.Data.Attributes       (mkAttributes)
import           Pos.Script                (Script)
import           Pos.Script.Examples       (multisigRedeemer, multisigValidator)
import           Pos.Txp                   (MonadUtxoRead (utxoGet), Tx (..), TxAux,
                                            TxDistribution (..), TxId, TxIn (..),
                                            TxInWitness (..), TxOut (..), TxOutAux (..),
                                            TxSigData, TxWitness, Utxo, UtxoStateT (..),
                                            applyTxToUtxo, filterUtxoByAddr, topsortTxs)
import           Pos.Types                 (Address, Block, ChainDifficulty, Coin,
                                            blockTxas, difficultyL, makePubKeyAddress,
                                            makeRedeemAddress, makeScriptAddress, mkCoin,
                                            sumCoins)

type TxInputs = NonEmpty TxIn
type TxOutputs = NonEmpty TxOutAux
type TxError = Text

-----------------------------------------------------------------------------
-- Tx creation
-----------------------------------------------------------------------------

-- | Generic function to create a transaction, given desired inputs, outputs and a
-- way to construct witness from signature data
makeAbstractTx :: (TxSigData -> TxInWitness) -> TxInputs -> TxOutputs -> TxAux
makeAbstractTx mkWit txInputs outputs = ( UnsafeTx txInputs txOutputs txAttributes
                                        , txWitness
                                        , txDist)
  where
    txOutputs = map toaOut outputs
    txAttributes = mkAttributes ()
    txOutHash = hash txOutputs
    txDist = TxDistribution (map toaDistr outputs)
    txDistHash = hash txDist
    txWitness = V.fromList $ toList $ map (mkWit . makeTxSigData) txInputs
    makeTxSigData TxIn{..} = (txInHash, txInIndex, txOutHash, txDistHash)

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx :: SafeSigner -> TxInputs -> TxOutputs -> TxAux
makePubKeyTx ss = makeAbstractTx mkWit
  where pk = safeToPublic ss
        mkWit sigData = PkWitness
            { twKey = pk
            , twSig = safeSign ss sigData
            }

makeMOfNTx :: Script -> [Maybe SafeSigner] -> TxInputs -> TxOutputs -> TxAux
makeMOfNTx validator sks = makeAbstractTx mkWit
  where mkWit sigData = ScriptWitness
            { twValidator = validator
            , twRedeemer = multisigRedeemer sigData sks
            }

makeRedemptionTx :: RedeemSecretKey -> TxInputs -> TxOutputs -> TxAux
makeRedemptionTx rsk = makeAbstractTx mkWit
  where rpk = redeemToPublic rsk
        mkWit sigData = RedeemWitness
            { twRedeemKey = rpk
            , twRedeemSig = redeemSign rsk sigData
            }

type FlatUtxo = [(TxIn, TxOutAux)]
type InputPicker = StateT (Coin, FlatUtxo) (Either TxError)

-- | Given Utxo, desired source address and desired outputs, prepare lists
-- of correct inputs and outputs to form a transaction
prepareInpOuts :: Utxo -> Address -> TxOutputs -> Either TxError (TxInputs, TxOutputs)
prepareInpOuts utxo addr outputs = do
    futxo <- evalStateT (pickInputs []) (totalMoney, sortedUnspent)
    let inputSum =
            unsafeIntegerToCoin $ sumCoins $ map (txOutValue . toaOut . snd) futxo
        newOuts
            | inputSum > totalMoney =
                TxOutAux (TxOut addr (inputSum `unsafeSubCoin` totalMoney)) [] <|
                outputs
            | otherwise = outputs
    case nonEmpty futxo of
        Nothing       -> fail "Failed to prepare inputs!"
        Just inputsNE -> pure (map fst inputsNE, newOuts)
  where
    totalMoney = unsafeIntegerToCoin $ sumCoins $ map (txOutValue . toaOut) outputs
    allUnspent = M.toList $ filterUtxoByAddr addr utxo
    sortedUnspent =
        sortBy (comparing $ Down . txOutValue . toaOut . snd) allUnspent
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
                        _1 %= unsafeSubCoin (min txOutValue moneyLeft)
                        _2 %= tail
                        pickInputs (inp : inps)


-- | Make a multi-transaction using given secret key and info for outputs
createTx :: Utxo -> SafeSigner -> TxOutputs -> Either TxError TxAux
createTx utxo ss outputs =
    uncurry (makePubKeyTx ss) <$>
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

----------------------------------------------------------------------
-- Deduction of history
----------------------------------------------------------------------

-- | Check if given 'Address' is one of the receivers of 'Tx'
hasReceiver :: Tx -> Address -> Bool
hasReceiver UnsafeTx {..} addr = any ((== addr) . txOutAddress) _txOutputs

-- | Given some 'Utxo', check if given 'Address' is one of the senders of 'Tx'
hasSender :: MonadUtxoRead m => Tx -> Address -> m Bool
hasSender UnsafeTx {..} addr = anyM hasCorrespondingOutput $ toList _txInputs
  where hasCorrespondingOutput txIn =
            fmap toBool $ fmap ((== addr) . txOutAddress . toaOut) <$> utxoGet txIn
        toBool Nothing  = False
        toBool (Just b) = b

-- | Datatype for returning info about tx history
data TxHistoryEntry = THEntry
    { _thTxId       :: !TxId
    , _thTx         :: !Tx
    , _thIsOutput   :: !Bool
    , _thDifficulty :: !(Maybe ChainDifficulty)
    } deriving (Show, Eq, Generic)

makeLenses ''TxHistoryEntry

-- | Type of monad used to deduce history
type TxSelectorT m = UtxoStateT (MaybeT m)

-- | Select transactions related to given address. `Bool` indicates
-- whether the transaction is outgoing (i. e. is sent from given address)
getRelatedTxs
    :: Monad m
    => Address
    -> [(WithHash Tx, TxWitness, TxDistribution)]
    -> TxSelectorT m [TxHistoryEntry]
getRelatedTxs addr txs = fmap DL.toList $
    lift (MaybeT $ return $ topsortTxs (view _1) txs) >>=
    foldlM step DL.empty
  where
    step ls (WithHash tx txId, _wit, dist) = do
        let isIncoming = tx `hasReceiver` addr
        isOutgoing <- tx `hasSender` addr
        let allToAddr = all ((== addr) . txOutAddress) $ _txOutputs tx
            isToItself = isOutgoing && allToAddr
        if isOutgoing || isIncoming
            then do
            applyTxToUtxo (WithHash tx txId) dist
            identity %= filterUtxoByAddr addr

            -- Workaround to present A to A transactions as a pair of self-canceling
            -- transactions in history
            let resEntry = THEntry txId tx isOutgoing Nothing
                resList = if isToItself
                          then DL.fromList [resEntry & thIsOutput .~ False, resEntry]
                          else DL.singleton resEntry
            return $ ls <> resList
            else return ls

-- | Given a full blockchain, derive address history and Utxo
-- TODO: Such functionality will still be useful for merging
-- blockchains when wallet state is ready, but some metadata for
-- Tx will be required.
deriveAddrHistory
    -- :: (Monad m, Ssc ssc) => Address -> [Block ssc] -> TxSelectorT m [TxHistoryEntry]
    :: (Monad m) => Address -> [Block ssc] -> TxSelectorT m [TxHistoryEntry]
deriveAddrHistory addr chain = identity %= filterUtxoByAddr addr >>
                               deriveAddrHistoryPartial [] addr chain

deriveAddrHistoryPartial
    :: (Monad m)
    => [TxHistoryEntry]
    -> Address
    -> [Block ssc]
    -> TxSelectorT m [TxHistoryEntry]
deriveAddrHistoryPartial hist addr chain =
    DL.toList <$> foldrM updateAll (DL.fromList hist) chain
  where
    updateAll (Left _) hst = pure hst
    updateAll (Right blk) hst = do
        txs <- getRelatedTxs addr $
                   map (over _1 withHash) (blk ^. blockTxas)
        let difficulty = blk ^. difficultyL
            txs' = map (thDifficulty .~ Just difficulty) txs
        return $ DL.fromList txs' <> hst
