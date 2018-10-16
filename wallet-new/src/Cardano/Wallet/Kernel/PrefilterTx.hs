{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Wallet.Kernel.PrefilterTx
       ( PrefilteredBlock(..)
       , PrefilteredUtxo
       , AddrWithId
       , prefilterBlock
       , prefilterUtxo
       ) where

import           Universum

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (listJson, mapJson)

import           Data.SafeCopy (base, deriveSafeCopy)

import           Pos.Core (Address (..))
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (EncryptedSecretKey, ProtocolMagic)
import           Pos.Txp.Toil.Types (Utxo)
import           Pos.Wallet.Web.State.Storage (WAddressMeta (..))
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials, eskToWalletDecrCredentials,
                                                  selectOwnAddresses)

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, ResolvedInput, ResolvedTx, rbTxs,
                                                    rtxInputs, rtxOutputs)
import           Cardano.Wallet.Kernel.Types (WalletId (..))

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs to those that belong to the given Wallet.
+-------------------------------------------------------------------------------}

-- | Extended Utxo with each output paired with an HdAddressId, required for
--   discovering new Addresses during prefiltering
type UtxoWithAddrId = Map TxIn (TxOutAux,HdAddressId)

-- | Address extended with an HdAddressId, which embeds information that places
--   the Address in the context of the Wallet/Accounts/Addresses hierarchy.
type AddrWithId = (HdAddressId,Address)

-- | Utxo along with all (extended) addresses ocurring in the Utxo
type PrefilteredUtxo = (Utxo,[AddrWithId])

-- | Prefiltered block
--
-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
data PrefilteredBlock = PrefilteredBlock {
      -- | Relevant inputs
      pfbInputs  :: Set TxIn

      -- | Relevant outputs
    , pfbOutputs :: Utxo

      -- | all output addresses present in the Utxo
    , pfbAddrs   :: [AddrWithId]
    }

deriveSafeCopy 1 'base ''PrefilteredBlock

type WalletKey = (WalletId, WalletDecrCredentials)

toPrefilteredUtxo :: UtxoWithAddrId -> PrefilteredUtxo
toPrefilteredUtxo utxoWithAddrs = (Map.fromList utxo', addrs')
    where
        toUtxo  (txIn,(txOutAux,_))         = (txIn,txOutAux)
        toAddrs (_   ,(txOutAux,addressId)) = (addressId, txOutAddress . toaOut $ txOutAux)

        utxoWithAddrs' = Map.toList utxoWithAddrs
        utxo'  = map toUtxo  utxoWithAddrs'
        addrs' = map toAddrs utxoWithAddrs'

-- | Prefilter the transactions of a resolved block for the given wallet.
--
--   Returns prefiltered blocks indexed by HdAccountId.
prefilterBlock :: ProtocolMagic
               -> WalletId
               -> EncryptedSecretKey
               -> ResolvedBlock
               -> Map HdAccountId PrefilteredBlock
prefilterBlock pm wid esk block
    = Map.fromList $ map mkPrefBlock (Set.toList accountIds)
  where
    mkPrefBlock accId'
        = (accId', PrefilteredBlock inps' outs' addrs')
        where
            byAccountId accId'' def dict = fromMaybe def $ Map.lookup accId'' dict

            inps'           =                    byAccountId accId' Set.empty inpAll
            (outs', addrs') = toPrefilteredUtxo (byAccountId accId' Map.empty outAll)

    nm :: NetworkMagic
    nm = makeNetworkMagic pm

    wdc :: WalletDecrCredentials
    wdc = eskToWalletDecrCredentials nm esk
    wKey = (wid, wdc)

    inps :: [Map HdAccountId (Set TxIn)]
    outs :: [Map HdAccountId UtxoWithAddrId]
    (inps, outs) = unzip $ map (prefilterTx wKey) (block ^. rbTxs)

    inpAll :: Map HdAccountId (Set TxIn)
    outAll :: Map HdAccountId UtxoWithAddrId
    inpAll = Map.unionsWith Set.union inps
    outAll = Map.unionsWith Map.union outs

    accountIds = Map.keysSet inpAll `Set.union` Map.keysSet outAll

-- | Prefilter the inputs and outputs of a resolved transaction
prefilterTx :: WalletKey
            -> ResolvedTx
            -> (Map HdAccountId (Set TxIn), Map HdAccountId UtxoWithAddrId)
prefilterTx wKey tx = (
      prefilterInputs wKey (toList (tx ^. rtxInputs . fromDb))
    , prefilterUtxo'  wKey (tx ^. rtxOutputs . fromDb)
    )

-- | Prefilter inputs of a transaction
prefilterInputs :: WalletKey
          -> [(TxIn, ResolvedInput)]
          -> Map HdAccountId (Set TxIn)
prefilterInputs wKey inps
    = Map.fromListWith Set.union
      $ map f
      $ prefilterResolvedTxPairs wKey inps
    where
        f (addressId, (txIn, _txOut)) = (addressId ^. hdAddressIdParent, Set.singleton txIn)

-- | Prefilter utxo using wallet key
prefilterUtxo' :: WalletKey -> Utxo -> Map HdAccountId UtxoWithAddrId
prefilterUtxo' wid utxo
    = Map.fromListWith Map.union
      $ map f
      $ prefilterResolvedTxPairs wid (Map.toList utxo)
    where
        f (addressId, (txIn, txOut)) = (addressId ^. hdAddressIdParent,
                                        Map.singleton txIn (txOut, addressId))

-- | Prefilter utxo using walletId and esk
prefilterUtxo :: ProtocolMagic -> HdRootId -> EncryptedSecretKey
              -> Utxo -> Map HdAccountId PrefilteredUtxo
prefilterUtxo pm rootId esk utxo = map toPrefilteredUtxo (prefilterUtxo' wKey utxo)
    where
        nm = makeNetworkMagic pm
        wKey = (WalletIdHdRnd rootId, eskToWalletDecrCredentials nm esk)

-- | Prefilter resolved transaction pairs
prefilterResolvedTxPairs :: WalletKey
                         -> [(TxIn, TxOutAux)]
                         -> [(HdAddressId, (TxIn, TxOutAux))]
prefilterResolvedTxPairs wid xs = map f $ prefilter wid selectAddr xs
    where
        f ((txIn, txOut), addressId) = (addressId, (txIn, txOut))
        selectAddr = txOutAddress . toaOut . snd

-- | Filter items for addresses that were derived from the given WalletKey.
--   Returns the matching HdAddressId, which embeds the parent HdAccountId
--   discovered for the matching item.
--
-- TODO(@uroboros/ryan) `selectOwnAddresses` calls `decryptAddress`, which extracts
-- the AccountId from the Tx Attributes. This is not sufficient since it
-- doesn't actually _verify_ that the Tx belongs to the AccountId.
-- We need to add verification (see `deriveLvl2KeyPair`).
prefilter :: WalletKey
     -> (a -> Address)      -- ^ address getter
     -> [a]                 -- ^ list to filter
     -> [(a, HdAddressId)]  -- ^ matching items
prefilter (wid,wdc) selectAddr rtxs
    = map f $ selectOwnAddresses wdc selectAddr rtxs
    where f (addr,meta) = (addr, toAddressId wid meta)

          toAddressId :: WalletId -> WAddressMeta -> HdAddressId
          toAddressId (WalletIdHdRnd rootId) meta' = addressId
              where
                  accountIx = HdAccountIx (_wamAccountIndex meta')
                  accountId = HdAccountId rootId accountIx

                  addressIx = HdAddressIx (_wamAddressIndex meta')
                  addressId = HdAddressId accountId addressIx

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
