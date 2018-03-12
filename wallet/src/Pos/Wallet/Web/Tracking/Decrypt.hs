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

type OwnTxInOuts = [((TxIn, TxOutAux), CWAddressMeta)]

-- | Auxiliary datatype which holds TxIns and TxOuts
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

isTxEntryInteresting :: THEntryExtra -> Maybe TxHistoryEntry
isTxEntryInteresting THEntryExtra{..} =
    if (not $ null theeInputs) || (not $ null theeOutputs) then Just theeTxEntry
    else Nothing

buildTHEntryExtra
    :: WalletDecrCredentials
    -> (WithHash Tx, TxUndo)
    -> (Maybe ChainDifficulty, Maybe Timestamp)
    -> THEntryExtra
buildTHEntryExtra wdc (WithHash tx txId, NE.toList -> undoL) (mDiff, mTs) =
    let (UncheckedTx (NE.toList -> inps) (NE.toList -> outs) _) = tx
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

type WalletDecrCredentials = (HDPassphrase, CId Wal)

eskToWalletDecrCredentials :: EncryptedSecretKey -> WalletDecrCredentials
eskToWalletDecrCredentials encSK = do
    let pubKey = encToPublic encSK
    let hdPass = deriveHDPassphrase pubKey
    let wCId = encodeCType $ makeRootPubKeyAddress pubKey
    (hdPass, wCId)

selectOwnAddresses
    :: WalletDecrCredentials
    -> (a -> Address)
    -> [a]
    -> [(a, CWAddressMeta)]
selectOwnAddresses wdc getAddr =
    mapMaybe (\a -> (a,) <$> decryptAddress wdc (getAddr a))

decryptAddress :: WalletDecrCredentials -> Address -> Maybe CWAddressMeta
decryptAddress (hdPass, wCId) addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure $ CWAddressMeta wCId (derPath !! 0) (derPath !! 1) (encodeCType addr)
