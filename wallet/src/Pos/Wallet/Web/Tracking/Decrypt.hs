-- | Contains some utils to decrypt HD payload of addresses.
-- Also utils to create THEntryExtra which based on
-- decrypting of HDPayload.

module Pos.Wallet.Web.Tracking.Decrypt
       ( THEntryExtra (..)
       , isTxEntryInteresting
       , buildTHEntryExtra

       , WalletDecrCredentials
       , keyToWalletDecrCredentials
       , selectOwnAddresses
       , decryptAddress
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
import           Pos.Crypto (EncryptedSecretKey, PublicKey, HDPassphrase, WithHash (..),
                             deriveHDPassphrase, encToPublic, unpackHDAddressAttr)
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.Web.ClientTypes (CId, Wal)
import           Pos.Wallet.Web.State (WAddressMeta(..))

type OwnTxInOuts = [((TxIn, TxOutAux), WAddressMeta)]

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
    let (UnsafeTx (NE.toList -> inps) (NE.toList -> outs) _) = tx
        toTxInOut (idx, out) = (TxInUtxo txId idx, TxOutAux out)

        resolvedInputs :: [(TxIn, TxOutAux)]
        resolvedInputs = catMaybes (zipWith (fmap . (,)) inps undoL)
        txOutgoings = map txOutAddress outs
        txInputs = map (toaOut . snd) resolvedInputs

        theeInputs :: [((TxIn, TxOutAux), WAddressMeta)]
        theeInputs = selectOwnAddresses wdc (txOutAddress . toaOut . snd) resolvedInputs
        theeOutputsRaw :: [((Word32, TxOut), WAddressMeta)]
        theeOutputsRaw = selectOwnAddresses wdc (txOutAddress . snd) (enumerate outs)
        theeOutputs = map (first toTxInOut) theeOutputsRaw
        theeTxEntry = THEntry txId tx mDiff txInputs txOutgoings mTs in
    THEntryExtra {..}

type WalletDecrCredentials = (HDPassphrase, CId Wal)

-- | There's a secret key for regular (internal) wallet or a public key for external one.
keyToWalletDecrCredentials :: Either PublicKey EncryptedSecretKey -> WalletDecrCredentials
keyToWalletDecrCredentials key = case key of
    Right encSecretKey -> credentialsFromPublicKey $ encToPublic encSecretKey
    Left publicKey     -> credentialsFromPublicKey publicKey
  where
    credentialsFromPublicKey publicKey =
        let hdPassword = deriveHDPassphrase publicKey
            walletCId  = encodeCType $ makeRootPubKeyAddress publicKey
        in (hdPassword, walletCId)

selectOwnAddresses
    :: WalletDecrCredentials
    -> (a -> Address)
    -> [a]
    -> [(a, WAddressMeta)]
selectOwnAddresses wdc getAddr =
    mapMaybe (\a -> (a,) <$> decryptAddress wdc (getAddr a))

decryptAddress :: WalletDecrCredentials -> Address -> Maybe WAddressMeta
decryptAddress (hdPass, wCId) addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure $ WAddressMeta wCId (derPath !! 0) (derPath !! 1) addr
