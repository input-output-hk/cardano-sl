-- | Contains some utils to decrypt HD payload of addresses.
-- Also utils to create THEntryExtra which based on
-- decrypting of HDPayload.

module Pos.Wallet.Web.Tracking.Decrypt
       ( THEntryExtra (..)
       , isTxEntryInteresting
       , buildTHEntryExtra

       , WalletDecrCredentials
       , eskToWalletDecrCredentials
       , selectOwnAddresses
       ) where

import           Universum

import           Data.List ((!!))
import qualified Data.List.NonEmpty as NE
import           Serokell.Util (enumerate)

import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Core (Address (..), ChainDifficulty, Timestamp, aaPkDerivationPath,
                           addrAttributesUnwrapped, makeRootPubKeyAddress)
import           Pos.Core.Txp (Tx (..), TxIn (..), TxOut, TxOutAux (..), TxUndo, toaOut,
                               txOutAddress)
import           Pos.Crypto (EncryptedSecretKey, HDPassphrase, WithHash (..), deriveHDPassphrase,
                             encToPublic, unpackHDAddressAttr)
import           Pos.Util.Servant (encodeCType)

import           Pos.Wallet.Web.ClientTypes (CId, CWAddressMeta (..), Wal)

-- | Pairs of mutually corresponding transaction inputs and outputs
-- together with corresponding address' metadata.
type OwnTxInOuts = [((TxIn, TxOutAux), CWAddressMeta)]

-- | Auxiliary datatype which holds @TxIn@s and @TxOut@s
-- belonging to some wallet.
data THEntryExtra = THEntryExtra
    { theeInputs  :: OwnTxInOuts
    -- ^ Inputs and corresponding outputs of tx
    -- which belong to wallet
    , theeOutputs :: OwnTxInOuts
    -- ^ Outputs and corresponding inputs of tx
    -- which belong to wallet
    , theeTxEntry :: TxHistoryEntry
    -- ^ Tx entry for history, likely it's not our entry
    }

-- | Determine if a transaction is somehow related to a wallet.
-- Transaction is related to wallet if wallet has at least one
-- own input among transaction inputs or at least one output
-- among transaction outputs.
isTxEntryInteresting :: THEntryExtra -> Maybe TxHistoryEntry
isTxEntryInteresting THEntryExtra{..} =
    if (not $ null theeInputs) || (not $ null theeOutputs) then Just theeTxEntry
    else Nothing

-- | In order to decrypt addresses' attributes, we need an 'HDPassphrase',
-- which is basically a 32-bytes bytestring. Wallet ID is provided together
-- with 'HDPassphrase' in order to form correct 'CWalletMeta' for successfully
-- decrypted addresses (see function 'decryptAddress').
type WalletDecrCredentials = (HDPassphrase, CId Wal)

-- | Given wallet decrypt credentials, a transaction, corresponding 'TxUndo'
-- and additional data necessary for forming history entry, select
-- inputs and outputs which belong to given wallet and form a
-- 'TxHistoryEntry' for transaction.
buildTHEntryExtra
    :: WalletDecrCredentials
    -> (WithHash Tx, TxUndo)
    -> (Maybe ChainDifficulty, Maybe Timestamp)
    -> THEntryExtra
buildTHEntryExtra wdc (WithHash tx txId, NE.toList -> undoL) (mDiff, mTs) =
    let (UnsafeTx (NE.toList -> inps) (NE.toList -> outs) _) = tx
        toTxInOut (idx, out) = (TxInUtxo txId idx, TxOutAux out)

        resolvedInputs :: [(TxIn, TxOutAux)]
        resolvedInputs = catMaybes (zipWith (fmap . (,)) inps undoL)
        txOutgoings = map txOutAddress outs
        txInputs = map (toaOut . snd) resolvedInputs

        theeInputs :: [((TxIn, TxOutAux), CWAddressMeta)]
        theeInputs = selectOwnAddresses wdc (txOutAddress . toaOut . snd) resolvedInputs
        theeOutputsRaw :: [((Word32, TxOut), CWAddressMeta)]
        theeOutputsRaw = selectOwnAddresses wdc (txOutAddress . snd) (enumerate outs)
        theeOutputs = map (first toTxInOut) theeOutputsRaw
        theeTxEntry = THEntry txId tx mDiff txInputs txOutgoings mTs in
    THEntryExtra {..}

-- | Derive wallet credentials from encrypted secret key.
-- Wallet ID is derived as address derived from public key.
-- 'HDPassphrase' is also derived from a public key. For details
-- of derivation process see 'Pos.Crypto.HD.deriveHDPassphrase'.
--
-- TODO: Deriving 'HDPassphrase' from root public key is a security concern
-- (because public key is meant to be, well, /public/).
-- It's suggested that we should better derive it from dedicated level 1
-- index in HD wallet tree.
--
-- TODO: In the meanwhile, if we need only /public/ key to derive
-- decrypt credentials, why do we pass a secret key here?
eskToWalletDecrCredentials :: EncryptedSecretKey -> WalletDecrCredentials
eskToWalletDecrCredentials encSK = do
    let pubKey = encToPublic encSK
    let hdPass = deriveHDPassphrase pubKey
    let wCId = encodeCType $ makeRootPubKeyAddress pubKey
    (hdPass, wCId)

-- | Select addresses which belong to given wallet and provide
-- corresponding 'CWAddressMeta's for these addresses.
-- Function is written in generic way (we filter not exactly addresses,
-- but some objects which contain addresses instead) in order to
-- avoid cumbersome data packing\/unpacking.
selectOwnAddresses
    :: WalletDecrCredentials
    -> (a -> Address)
    -> [a]
    -> [(a, CWAddressMeta)]
selectOwnAddresses wdc getAddr =
    mapMaybe (\a -> (a,) <$> decryptAddress wdc (getAddr a))

-- | Given wallet decrypt credentials, try to decrypt address attributes --
-- if they are decrypted successfully and contain valid data, then
-- this address belong to given wallet. Return corresponding 'CWAddressMeta'
-- in this case, and 'Nothing' otherwise.
decryptAddress :: WalletDecrCredentials -> Address -> Maybe CWAddressMeta
decryptAddress (hdPass, wCId) addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure $ CWAddressMeta wCId (derPath !! 0) (derPath !! 1) (encodeCType addr)
