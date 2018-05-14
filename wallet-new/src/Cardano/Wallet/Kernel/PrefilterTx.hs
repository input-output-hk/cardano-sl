{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Wallet.Kernel.PrefilterTx
       ( PrefilteredBlock(..)
       , prefilterBlock
       , ourUtxo
       ) where

import           Universum

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (listJson, mapJson)

import           Pos.Core (Address (..), HasConfiguration)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.Txp.Toil.Types (Utxo)
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials, eskToWalletDecrCredentials,
                                                  selectOwnAddresses)

import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, ResolvedTx, rbTxs, rtxInputs,
                                                    rtxOutputs)

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs to those that belong to the given Wallet.
+-------------------------------------------------------------------------------}

-- | Prefiltered block
--
-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
data PrefilteredBlock = PrefilteredBlock {
      -- | Relevant inputs
      pfbInputs  :: Set TxIn

      -- | Relevant outputs
    , pfbOutputs :: Utxo
    }

prefilterBlock :: HasConfiguration
               => EncryptedSecretKey
               -> ResolvedBlock
               -> PrefilteredBlock
prefilterBlock esk block = PrefilteredBlock {
      pfbInputs  = Set.fromList . map fst $ concat inpss
    , pfbOutputs = Map.unions outss
    }
  where
    inpss :: [[(TxIn, TxOutAux)]]
    outss :: [Utxo]
    (inpss, outss) = unzip $ map (prefilterTx wdc) (block ^. rbTxs)

    wdc :: WalletDecrCredentials
    wdc = eskToWalletDecrCredentials esk

prefilterTx :: WalletDecrCredentials
            -> ResolvedTx
            -> ([(TxIn, TxOutAux)], Utxo)
prefilterTx wdc tx = (
      ourResolvedTxPairs wdc (toList (tx ^. rtxInputs  . fromDb))
    , ourUtxo_           wdc         (tx ^. rtxOutputs . fromDb)
    )

ourResolvedTxPairs :: WalletDecrCredentials
                   -> [(TxIn, TxOutAux)]
                   -> [(TxIn, TxOutAux)]
ourResolvedTxPairs wdc = ours wdc (txOutAddress . toaOut . snd)

ourUtxo :: EncryptedSecretKey -> Utxo -> Utxo
ourUtxo esk = ourUtxo_ $ eskToWalletDecrCredentials esk

ourUtxo_ :: WalletDecrCredentials -> Utxo -> Utxo
ourUtxo_ wdc utxo = Map.fromList $ ourResolvedTxPairs wdc $ Map.toList utxo

ours :: WalletDecrCredentials
     -> (a -> Address)
     -> [a]
     -> [a]
ours wdc selectAddr rtxs = map fst $ selectOwnAddresses wdc selectAddr rtxs

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable PrefilteredBlock where
  build PrefilteredBlock{..} = bprint
    ( "PrefilteredBlock "
    % "{ inputs:  " % listJson
    % ", outputs: " % mapJson
    % "}"
    )
    (Set.toList pfbInputs)
    pfbOutputs
