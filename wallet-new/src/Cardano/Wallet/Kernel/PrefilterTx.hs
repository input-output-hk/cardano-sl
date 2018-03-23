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
import           Pos.Core.Txp (TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil.Types (Utxo)
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials,
                                                  eskToWalletDecrCredentials,
                                                  selectOwnAddresses)

import           Cardano.Wallet.Kernel.Types (ResolvedTx(..), ResolvedTxPair)

{-------------------------------------------------------------------------------
Pre-filter Tx Inputs and Outputs to "Ours" i.e. those that
belong to the given Wallet.
-------------------------------------------------------------------------------}

prefilterTxs
    :: HasConfiguration
    => EncryptedSecretKey    -- ^ Wallet's secret key
    -> [ResolvedTx]
    -> [ResolvedTx]          -- ^ Prefiltered [(inputs, outputs)]
prefilterTxs (eskToWalletDecrCredentials -> wdc)
    = map (prefilterTx wdc)

prefilterTx :: WalletDecrCredentials
             -> ResolvedTx
             -> ResolvedTx
prefilterTx wdc ResolvedTx{..} =
    ResolvedTx  (ourResolvedTxPairs wdc rtxInputs)
                (ourUtxo_ wdc rtxOutputs)

ourResolvedTxPairs :: WalletDecrCredentials -> [ResolvedTxPair] -> [ResolvedTxPair]
ourResolvedTxPairs wdc = ours wdc (txOutAddress . toaOut . snd)

ourUtxo :: EncryptedSecretKey -> Utxo -> Utxo
ourUtxo (eskToWalletDecrCredentials -> wdc) = ourUtxo_ wdc

ourUtxo_ :: WalletDecrCredentials -> Utxo -> Utxo
ourUtxo_ wdc utxo = Map.fromList $ ourResolvedTxPairs wdc $ Map.toList utxo

ours :: WalletDecrCredentials
        -> (a -> Address)
        -> [a]
        -> [a]
ours wdc selectAddr rtxs = map fst $ selectOwnAddresses wdc selectAddr rtxs
