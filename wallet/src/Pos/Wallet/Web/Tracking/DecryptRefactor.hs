-- | Contains some utils to decrypt HD payload of addresses.
-- Also utils to create THEntryExtra which based on
-- decrypting of HDPayload.

module Pos.Wallet.Web.Tracking.DecryptRefactor
       ( THEntryExtra (..)
       , isTxEntryInteresting
       , buildTHEntryExtra

       , WalletDecrCredentials
       , eskToWalletDecrCredentials
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
import           Pos.Crypto (EncryptedSecretKey, HDPassphrase, WithHash (..), deriveHDPassphrase,
                             encToPublic, unpackHDAddressAttr)
import           Pos.Util.Servant (encodeCType, OriginType, ToCType)

type OwnTxInOuts w = [((TxIn, TxOutAux), w)]

-- | Auxiliary datatype which holds TxIns and TxOuts
-- belonging to some wallet.
data THEntryExtra w = THEntryExtra
    { theeInputs  :: OwnTxInOuts w
    -- ^ Inputs and corresponding outputs of tx
    -- which belong to wallet
    , theeOutputs :: OwnTxInOuts w
    -- ^ Outputs and corresponding inputs of tx
    -- which belong to wallet
    , theeTxEntry :: TxHistoryEntry
    -- ^ Tx entry for history, likely it's not our entry
    }

isTxEntryInteresting :: THEntryExtra w -> Maybe TxHistoryEntry
isTxEntryInteresting THEntryExtra{..} =
    if not (null theeInputs) || not (null theeOutputs) then Just theeTxEntry
    else Nothing

buildTHEntryExtra
    :: forall w. Maker Address w
    -> WalletDecrCredentials Address
    -> (WithHash Tx, TxUndo)
    -> (Maybe ChainDifficulty, Maybe Timestamp)
    -> THEntryExtra w
buildTHEntryExtra wAddressMeta wdc (WithHash tx txId, NE.toList -> undoL) (mDiff, mTs) =
    let (UnsafeTx (NE.toList -> inps) (NE.toList -> outs) _) = tx
        toTxInOut (idx, out) = (TxInUtxo txId idx, TxOutAux out)

        resolvedInputs :: [(TxIn, TxOutAux)]
        resolvedInputs = catMaybes (zipWith (fmap . (,)) inps undoL)
        txOutgoings = map txOutAddress outs
        txInputs = map (toaOut . snd) resolvedInputs

        theeInputs :: [((TxIn, TxOutAux), w)]
        theeInputs = selectOwnAddresses wAddressMeta wdc (txOutAddress . toaOut . snd) resolvedInputs
        theeOutputsRaw :: [((Word32, TxOut), w)]
        theeOutputsRaw = selectOwnAddresses wAddressMeta wdc (txOutAddress . snd) (enumerate outs)
        theeOutputs = map (first toTxInOut) theeOutputsRaw
        theeTxEntry = THEntry txId tx mDiff txInputs txOutgoings mTs in
    THEntryExtra {..}

type WalletDecrCredentials x = (HDPassphrase, x)

type Maker a r = a -> Word32 -> Word32 -> Address -> r

eskToWalletDecrCredentials
    :: (Address ~ OriginType x, ToCType x)
    => EncryptedSecretKey
    -> WalletDecrCredentials x
eskToWalletDecrCredentials encSK = do
    let pubKey = encToPublic encSK
    let hdPass = deriveHDPassphrase pubKey
    let wCId = encodeCType $ makeRootPubKeyAddress pubKey
    (hdPass, wCId)

selectOwnAddresses
    :: Maker Address w
    -> WalletDecrCredentials Address
    -> (a -> Address)
    -> [a]
    -> [(a, w)]
selectOwnAddresses wAddressMeta wdc getAddr =
    mapMaybe (\a -> (a,) <$> decryptAddress wAddressMeta wdc (getAddr a))

decryptAddress :: Maker x w -> WalletDecrCredentials x -> Address -> Maybe w
decryptAddress wAddressMeta (hdPass, wCId) addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure $ wAddressMeta wCId (derPath !! 0) (derPath !! 1) addr
