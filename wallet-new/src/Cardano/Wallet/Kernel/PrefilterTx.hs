{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Wallet.Kernel.PrefilterTx
       ( PrefilteredBlock(..)
       , emptyPrefilteredBlock
       , AddrWithId
       , prefilterBlock
       , prefilterUtxo
       ) where

import           Universum

import           Data.List (nub)
import qualified Data.List.NonEmpty as NE

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Formatting (bprint, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson, mapJson)

import           Data.SafeCopy (base, deriveSafeCopy)

import           Pos.Chain.Txp (Utxo)
import           Pos.Core (Address (..), SlotId)
import           Pos.Core.Txp (TxId, TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.Wallet.Web.State.Storage (WAddressMeta (..))
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials,
                     eskToWalletDecrCredentials, selectOwnAddresses)

import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock,
                     ResolvedInput, ResolvedTx, rbSlotId, rbTxs, rtxInputs,
                     rtxOutputs)

import           Cardano.Wallet.Kernel.Types (WalletId (..))

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs; pre-filter a block of transactions.
+-------------------------------------------------------------------------------}

-- | Address extended with an HdAddressId, which embeds information that places
--   the Address in the context of the Wallet/Accounts/Addresses hierarchy.
type AddrWithId = (HdAddressId,Address)

-- | Prefiltered block
--
-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
data PrefilteredBlock = PrefilteredBlock {
      -- | Relevant inputs
      pfbInputs  :: !(Set TxIn)

      -- | Relevant outputs
    , pfbOutputs :: !Utxo

      -- | all output addresses present in the Utxo
    , pfbAddrs   :: ![AddrWithId]

      -- | Prefiltered block metadata
    , pfbMeta    :: !LocalBlockMeta
    }

deriveSafeCopy 1 'base ''PrefilteredBlock

-- | Empty prefiltered block
--
-- An empty prefiltered block is what we get when we filter a block for a
-- particular account and there is nothing in the block that is of
-- relevance to that account
emptyPrefilteredBlock :: PrefilteredBlock
emptyPrefilteredBlock = PrefilteredBlock {
      pfbInputs  = Set.empty
    , pfbOutputs = Map.empty
    , pfbAddrs   = []
    , pfbMeta    = emptyLocalBlockMeta
    }

type WalletKey = (WalletId, WalletDecrCredentials)

-- | Summary of an address as it appears in a transaction.
--   NOTE: Since an address can occur in multiple transactions, there could be
--   multiple valid summaries for an address.
data AddressSummary = AddressSummary {
      addrSummaryAddr        :: Address
    ,
      addrSummaryId          :: HdAddressId
    ,
      addrSummaryTxId        :: TxId
    ,
      -- | indicates whether _all_ the inputs of the transaction are "ours"
      addrSummaryOnlyOurInps :: Bool
    ,
      -- | indicates whether _all_ the outputs of the transaction are "ours"
      addrSummaryOnlyOurOuts :: Bool
    }

-- | Extended Utxo with each output paired with an HdAddressId, required for
--   discovering new Addresses during prefiltering
type UtxoWithAddrId = Map TxIn (TxOutAux,HdAddressId)

-- | Extended Utxo where each output is paired with an AddressSummary. Provides
--   the required metadata for computing address meta data for BlockMeta.
type UtxoSummaryRaw = Map TxIn (TxOutAux,AddressSummary)

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs to those that belong to the given Wallet.
+-------------------------------------------------------------------------------}

-- | Prefilter the inputs and outputs of a resolved transaction.
--   Prefiltered inputs and outputs are indexed by accountId.
--   The output Utxo is extended with address summary information
prefilterTx :: WalletKey
            -> ResolvedTx
            -> (Map HdAccountId (Set TxIn)      -- prefiltered inputs
              , Map HdAccountId UtxoSummaryRaw) -- prefiltered output utxo, extended with address summary
prefilterTx wKey tx = (prefInps,prefOuts')
    where
        inps = toList (tx ^. rtxInputs  . fromDb)
        outs =         tx ^. rtxOutputs . fromDb

        (onlyOurInps,prefInps) = prefilterInputs wKey inps
        (onlyOurOuts,prefOuts) = prefilterUtxo'  wKey outs

        prefOuts' = Map.map (extendWithSummary (onlyOurInps,onlyOurOuts))
                            prefOuts

-- | Prefilter inputs of a transaction
prefilterInputs :: WalletKey
          -> [(TxIn, ResolvedInput)]
          -> (Bool, Map HdAccountId (Set TxIn))
prefilterInputs wKey inps
    = prefilterResolvedTxPairs wKey mergeF inps
    where
        mergeF = Map.fromListWith Set.union . (map f)

        f ((txIn, _txOut),addrId) = (addrId ^. hdAddressIdParent,
                                     Set.singleton txIn)

-- | Prefilter utxo using wallet key
prefilterUtxo' :: WalletKey -> Utxo -> (Bool, Map HdAccountId UtxoWithAddrId)
prefilterUtxo' wKey utxo
    = prefilterResolvedTxPairs wKey mergeF (Map.toList utxo)
    where
        mergeF = Map.fromListWith Map.union . (map f)

        f ((txIn, txOut),addrId) = (addrId ^. hdAddressIdParent,
                                    Map.singleton txIn (txOut, addrId))

-- | Prefilter utxo using walletId and esk
prefilterUtxo :: HdRootId -> EncryptedSecretKey -> Utxo -> Map HdAccountId (Utxo,[AddrWithId])
prefilterUtxo rootId esk utxo = map toPrefilteredUtxo prefUtxo
    where
        (_,prefUtxo) = prefilterUtxo' wKey utxo
        wKey         = (WalletIdHdRnd rootId, eskToWalletDecrCredentials esk)

-- | Produce Utxo along with all (extended) addresses occurring in the Utxo
toPrefilteredUtxo :: UtxoWithAddrId -> (Utxo,[AddrWithId])
toPrefilteredUtxo utxoWithAddrs = (Map.fromList utxoL, addrs)
    where
        toUtxo (txIn,(txOutAux,_))         = (txIn,txOutAux)
        toAddr (_   ,(txOutAux,addressId)) = (addressId, txOutAddress . toaOut $ txOutAux)

        toSummary :: (TxIn,(TxOutAux,HdAddressId))
                  -> ((TxIn,TxOutAux), AddrWithId)
        toSummary item = (toUtxo item, toAddr item)

        utxoSummary = map toSummary $ Map.toList utxoWithAddrs
        (utxoL, addrs) = unzip utxoSummary

-- | Prefilter resolved transaction pairs.
--   Also returns a Boolean indicating whether @all@ pairs are "ours"
prefilterResolvedTxPairs :: WalletKey
                         -> ([((TxIn, TxOutAux), HdAddressId)] -> a)
                         -> [(TxIn, TxOutAux)]
                         -> (Bool, a)
prefilterResolvedTxPairs wKey mergeF pairs
    = (onlyOurs, mergeF prefTxPairs)
    where
        selectAddr = txOutAddress . toaOut . snd

        prefTxPairs = prefilter wKey selectAddr pairs
        -- | if prefiltering excluded nothing, then all the pairs are "ours"
        onlyOurs = (length prefTxPairs == length pairs)

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

extendWithSummary :: (Bool, Bool)
                  -- ^ Bools that indicate whether the inputs and outsputs are all "ours"
                  -> Map TxIn (TxOutAux,HdAddressId)
                  -- ^ Utxo extended with HdAddressId
                  -> Map TxIn (TxOutAux,AddressSummary)
                  -- ^ Utxo extended with AddressSummary
extendWithSummary (onlyOurInps,onlyOurOuts) utxoWithAddrId
    = Map.fromList $ mapMaybe toAddrSummary (Map.toList utxoWithAddrId)
    where
        toAddrSummary (txIn,(txOutAux,addressId))
            = case txIn of
                (TxInUtxo txId _) -> Just (txIn,(txOutAux,addrSummary txId))
                (TxInUnknown _ _) -> Nothing -- NOTE: we ignore addresses with 'unknown' inputs
            where
                addrSummary txId' = AddressSummary (txOutAddress . toaOut $ txOutAux)
                                                    addressId
                                                    txId'
                                                    onlyOurInps
                                                    onlyOurOuts

{-------------------------------------------------------------------------------
 Pre-filter a block of transactions, adorn each prefiltered block with block metadata
+-------------------------------------------------------------------------------}

-- | Prefilter the transactions of a resolved block for the given wallet.
--
--   Returns prefiltered blocks indexed by HdAccountId.
prefilterBlock :: ResolvedBlock
               -> WalletId
               -> EncryptedSecretKey
               -> Map HdAccountId PrefilteredBlock
prefilterBlock block wid esk =
      Map.fromList
    $ map (mkPrefBlock (block ^. rbSlotId) inpAll outAll)
    $ Set.toList accountIds
  where
    wdc :: WalletDecrCredentials
    wdc  = eskToWalletDecrCredentials esk
    wKey = (wid, wdc)

    inps :: [Map HdAccountId (Set TxIn)]
    outs :: [Map HdAccountId UtxoSummaryRaw]
    (inps, outs) = unzip $ map (prefilterTx wKey) (block ^. rbTxs)

    inpAll :: Map HdAccountId (Set TxIn)
    outAll :: Map HdAccountId UtxoSummaryRaw
    inpAll = Map.unionsWith Set.union inps
    outAll = Map.unionsWith Map.union outs

    accountIds = Map.keysSet inpAll `Set.union` Map.keysSet outAll

mkPrefBlock :: SlotId
            -> Map HdAccountId (Set TxIn)
            -> Map HdAccountId (Map TxIn (TxOutAux, AddressSummary))
            -> HdAccountId
            -> (HdAccountId, PrefilteredBlock)
mkPrefBlock slotId inps outs accId = (accId, PrefilteredBlock {
        pfbInputs  = inps'
      , pfbOutputs = outs'
      , pfbAddrs   = addrs''
      , pfbMeta    = blockMeta'
      })
    where
        fromAddrSummary :: AddressSummary -> AddrWithId
        fromAddrSummary AddressSummary{..} = (addrSummaryId,addrSummaryAddr)

        byAccountId accId'' def dict = fromMaybe def $ Map.lookup accId'' dict

        inps'           =                  byAccountId accId Set.empty inps
        (outs', addrs') = fromUtxoSummary (byAccountId accId Map.empty outs)

        addrs''    = nub $ map fromAddrSummary addrs'
        blockMeta' = mkBlockMeta slotId addrs'

mkBlockMeta :: SlotId -> [AddressSummary] -> LocalBlockMeta
mkBlockMeta slotId addrs_ = LocalBlockMeta BlockMeta{..}
    where
        txIds' = nub $ map addrSummaryTxId addrs_

        indexedAddrs = indexByAddr addrs_

        _blockMetaSlotId      = InDb . Map.fromList . map (,slotId) $ txIds'
        _blockMetaAddressMeta = InDb $ Map.map mkAddressMeta indexedAddrs

-- | This function is called once for each address found in a particular block of
--   transactions. The collection of address summaries passed to this function
--   corresponds to occurances of a given address in transactions in a block.
--   Since the collection was made by indexing the block of transactions by address,
--   we can be sure that the address occurs in at least one transaction and
--   hence that there are at least one or more summaries passed to this function
--   for a given address.
mkAddressMeta :: NE.NonEmpty AddressSummary -> AddressMeta
mkAddressMeta addrs
    = AddressMeta isUsed isChange
    where
        occurs = NE.length addrs

        -- An address is considered "used" if
        -- (1) it is "our" address: we are only dealing with prefiltered transactions
        --     here and can at this stage assume that the address is indeed "ours".
        -- (2) the transaction is confirmed: we are dealing here with transactions that
        --     appear in a block and can assume that they are confirmed.
        isUsed = True

        -- An address is considered "change" if
        -- (1) it is "our" address: as with `isUsed` above, we can assume the address is "ours"
        -- (2) the address occurs in exactly one transaction in this block
        -- (3) for the (single) transaction in which this address appears, the
        --     outputs must not all be to "our" addresses (the transaction must have
        --     an output to at least one address that is not "ours")
        -- (4) all the inputs of the transaction in which this address appears
        --     must be "ours"
        isChange = (occurs == 1)                    -- (2)
                    && addrSummaryOnlyOurInps       -- (3)
                    && not addrSummaryOnlyOurOuts   -- (4)
            where AddressSummary{..} = NE.head addrs

-- | Index the list of address summaries by Address.
--   NOTE: Since there will be at least one AddressSummary per Address,
--   we can safely use NE.fromList.
indexByAddr :: [AddressSummary] -> Map Address (NE.NonEmpty AddressSummary)
indexByAddr addrs
    -- TODO @uroboros/ryan construct NE lists and use NE.concat (would need NE.concat)
    = Map.map NE.fromList (Map.fromListWith (++) addrs')
    where
        fromAddrSummary addrSummary = (addrSummaryAddr addrSummary, [addrSummary])
        addrs' = map fromAddrSummary addrs

fromUtxoSummary :: Map TxIn (TxOutAux,AddressSummary)
                -> (Utxo,[AddressSummary])
fromUtxoSummary summary = (Map.fromList utxoL, addrs)
    where
        toUtxo (txIn,(txOutAux,_))           = (txIn,txOutAux)
        toAddr (_   ,(_       ,addrSummary)) = addrSummary

        unpackSummary item = (toUtxo item, toAddr item)

        (utxoL, addrs) = unzip $ map unpackSummary (Map.toList summary)

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
