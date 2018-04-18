{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Wallet.Kernel.PrefilterTx
       ( prefilterTxs
       , ourUtxo
       ) where

import           Universum
import qualified Data.Map as Map
import           Pos.Core (HasConfiguration, Address (..))
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil.Types (Utxo)
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials,
                                                  eskToWalletDecrCredentials,
                                                  selectOwnAddresses)

import           Cardano.Wallet.Kernel.Types (ResolvedTx(..))

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs to those that belong to the given Wallet.
+-------------------------------------------------------------------------------}

prefilterTxs
    :: HasConfiguration
    => EncryptedSecretKey    -- ^ Wallet's secret key
    -> [ResolvedTx]
    -> [ResolvedTx]          -- ^ Prefiltered [(inputs, outputs)]
prefilterTxs esk
    = map (prefilterTx wdc)
    where wdc = eskToWalletDecrCredentials esk

prefilterTx :: WalletDecrCredentials
             -> ResolvedTx
             -> ResolvedTx
prefilterTx wdc ResolvedTx{..} =
    ResolvedTx  (ourResolvedTxPairs wdc rtxInputs)
                (ourUtxo_ wdc rtxOutputs)

ourResolvedTxPairs :: WalletDecrCredentials
                   -> [(TxIn, TxOutAux)]
                   -> [(TxIn, TxOutAux)]
ourResolvedTxPairs wdc = ours wdc (txOutAddress . toaOut . snd)

ourUtxo :: EncryptedSecretKey -> Utxo -> Utxo
ourUtxo esk
    = ourUtxo_ $ eskToWalletDecrCredentials esk

ourUtxo_ :: WalletDecrCredentials -> Utxo -> Utxo
ourUtxo_ wdc utxo = Map.fromList $ ourResolvedTxPairs wdc $ Map.toList utxo

ours :: WalletDecrCredentials
        -> (a -> Address)
        -> [a]
        -> [a]
ours wdc selectAddr rtxs = map fst $ selectOwnAddresses wdc selectAddr rtxs
