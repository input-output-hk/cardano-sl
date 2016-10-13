{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Internal state of the transaction-handling worker.

module Pos.State.Storage.Tx
       (
         TxStorage
       , HasTxStorage(txStorage)

       , addTx
       ) where

import           Control.Lens  (makeClassy, use, (%=))
import           Data.Default  (Default, def)
import qualified Data.HashSet  as HS
import qualified Data.Map      as M
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Serokell.Util (isVerSuccess)
import           Universum

import           Pos.Crypto    (hash)
import           Pos.Genesis   (genesisUtxo)
import           Pos.Types     (Tx (..), TxOut (..), Utxo, deleteTxIn, verifyTxUtxo)

data TxStorage = TxStorage
    { -- | Local set of transactions. These are valid (with respect to
      -- utxo) transactions which are known to the node and are not
      -- included in the blockchain store by the node.
      _txLocalTxns :: !(HashSet Tx)
    , -- | Set of unspent transaction outputs. It is need to check new
      -- transactions and run follow-the-satoshi, for example.
      _txUtxo      :: !Utxo
    }

makeClassy ''TxStorage
deriveSafeCopySimple 0 'base ''TxStorage

instance Default TxStorage where
    def =
        TxStorage
        { _txLocalTxns = mempty
        , _txUtxo = genesisUtxo
        }

type Update a = forall m x. (HasTxStorage x, MonadState x m) => m a
type Query a = forall m x. (HasTxStorage x, MonadReader x m) => m a

-- | Add transaction to storage if it is fully valid. Returns True iff
-- transaction has been added.
addTx :: Tx -> Update Bool
addTx tx = do
    u <- use txUtxo
    let good = isVerSuccess $ verifyTxUtxo u tx
    good <$ when good (applyTx tx)

applyTx :: Tx -> Update ()
applyTx tx@Tx {..} = do
    txLocalTxns %= HS.insert tx
    mapM_ applyInput txInputs
    mapM_ applyOutput $ zip [0 ..] txOutputs
  where
    h = hash tx
    applyInput txIn = txUtxo %= deleteTxIn txIn
    applyOutput (idx, TxOut {..}) =
        txUtxo %= M.insert (h, idx, txOutValue) txOutAddress
