{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Wallet.Kernel.PrefilterTx
       ( PrefilteredBlock(..)
       , prefilterBlock
       , utxoForAccount
       , prefilterUtxo
       ) where

import           Universum

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (listJson, mapJson)

import           Data.SafeCopy (base, deriveSafeCopy)

import           Pos.Core (Address (..), HasConfiguration)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.Txp.Toil.Types (Utxo)
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials, eskToWalletDecrCredentials,
                                                  selectOwnAddresses)
import           Pos.Wallet.Web.State.Storage (WAddressMeta (..))

import           Cardano.Wallet.Kernel.Types(WalletId (..), accountToWalletId)
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, ResolvedInput, ResolvedTx, rbTxs, rtxInputs,
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

deriveSafeCopy 1 'base ''PrefilteredBlock

type WalletKey = (WalletId, WalletDecrCredentials)

-- | Prefilter the transactions of a resolved block for the given wallet.
--
--   Returns prefiltered blocks indexed by HdAccountId.
prefilterBlock :: HasConfiguration
               => WalletId
               -> EncryptedSecretKey
               -> ResolvedBlock
               -> Map HdAccountId PrefilteredBlock
prefilterBlock wid esk block
    = Map.fromList $ map mkPrefBlock (Set.toList accountIds)
  where
    mkPrefBlock accId' = (
                           accId'
                         , PrefilteredBlock (byAccountId accId' Set.empty inpAll)
                                            (byAccountId accId' Map.empty outAll)
                         )
    byAccountId accId' def dict = fromMaybe def $ Map.lookup accId' dict

    wdc :: WalletDecrCredentials
    wdc = eskToWalletDecrCredentials esk
    wKey = (wid, wdc)

    inpss :: [Map HdAccountId (Set TxIn)]
    outss :: [Map HdAccountId Utxo]
    (inpss, outss) = unzip $ map (prefilterTx wKey) (block ^. rbTxs)

    inpAll :: Map HdAccountId (Set TxIn)
    outAll :: Map HdAccountId Utxo
    inpAll = Map.unionsWith Set.union inpss
    outAll = Map.unionsWith Map.union outss

    accountIds = Map.keysSet inpAll `Set.union` Map.keysSet outAll

-- | Prefilter the inputs and outputs of a resolved transaction
prefilterTx :: WalletKey
            -> ResolvedTx
            -> (Map HdAccountId (Set TxIn), Map HdAccountId Utxo)
prefilterTx wKey tx = (
      ourInputs wKey (toList (tx ^. rtxInputs  . fromDb))
    , ourUtxo  wKey (tx ^. rtxOutputs . fromDb)
    )

-- | Prefilter inputs of a transaction
ourInputs :: WalletKey
          -> [(TxIn, ResolvedInput)]
          -> Map HdAccountId (Set TxIn)
ourInputs wKey inps = Map.fromListWith Set.union
                      $ map f
                      $ ourResolvedTxPairs wKey inps
    where
        f (accountId, (txIn, _txOut)) = (accountId, Set.singleton txIn)

-- | Prefilter utxo and return the utxo matching the given accountId (if it exists)
utxoForAccount :: HdAccountId -> EncryptedSecretKey -> Utxo -> Utxo
utxoForAccount accountId esk utxo
    = fromMaybe Map.empty $ Map.lookup accountId ourUtxo'
    where
        wid = accountToWalletId accountId
        ourUtxo' = ourUtxo (wid, eskToWalletDecrCredentials esk) utxo

-- | Prefilter utxo using wallet key
ourUtxo :: WalletKey -> Utxo -> Map HdAccountId Utxo
ourUtxo wid utxo = Map.fromListWith Map.union
                    $ map f
                    $ ourResolvedTxPairs wid (Map.toList utxo)
    where
        f (accountId, (txIn, txOut)) = (accountId, Map.singleton txIn txOut)

-- | Prefilter utxo using walletId and esk
prefilterUtxo :: HdRootId -> EncryptedSecretKey -> Utxo -> Map HdAccountId Utxo
prefilterUtxo rootId esk utxo = ourUtxo wKey utxo
    where
        wKey = (WalletIdHdRnd rootId, eskToWalletDecrCredentials esk)

-- | Prefilter resolved transaction pairs
ourResolvedTxPairs :: WalletKey
                   -> [(TxIn, TxOutAux)]
                   -> [(HdAccountId, (TxIn, TxOutAux))]
ourResolvedTxPairs wid xs = map f $ ours wid selectAddr xs
    where
        f ((txIn, txOut), accountId) = (accountId, (txIn, txOut))
        selectAddr = txOutAddress . toaOut . snd

-- | Filter items by filtering for 'our' addresses. Returns matches, along
--   with the HdAccountId discovered for the matching item
ours :: WalletKey
     -> (a -> Address)      -- ^ address getter
     -> [a]                 -- ^ list to filter
     -> [(a, HdAccountId)]  -- ^ matching items
ours (wid,wdc) selectAddr rtxs
    = map f $ selectOwnAddresses wdc selectAddr rtxs
    where f (addr,meta) = (addr, toAccountId wid meta)

          toAccountId :: WalletId -> WAddressMeta -> HdAccountId
          toAccountId (WalletIdHdRnd rootId) meta' = accountId
              where
                  accountIx = HdAccountIx (_wamAccountIndex meta')
                  accountId = HdAccountId rootId accountIx

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
