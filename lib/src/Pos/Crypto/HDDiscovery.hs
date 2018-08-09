-- | Functions which iterate over Utxo and search for
-- addresses which correspond to specified HD passphrases.

module Pos.Crypto.HDDiscovery
       ( discoverHDAddress
       , discoverHDAddresses
       ) where

import           Universum hiding (safeHead)

import           Data.Conduit (mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import           UnliftIO (MonadUnliftIO)

import           Pos.Core (AddrAttributes (..), Address (..), addrAttributesUnwrapped)
import           Pos.Core.Txp (toaOut, txOutAddress)
import           Pos.Crypto.HD (HDAddressPayload, HDPassphrase, unpackHDAddressAttr)
import           Pos.DB.Class (DBTag (GStateDB), MonadDBRead, dbIterSource)
import           Pos.Txp.DB (UtxoIter)

discoverHDAddress ::
       (MonadDBRead m, MonadUnliftIO m)
    => HDPassphrase
    -> m [(Address, [Word32])]
discoverHDAddress walletPassphrase =
    safeHead <$> discoverHDAddresses [walletPassphrase]
  where
    safeHead [x] = x
    safeHead _   = []

discoverHDAddresses ::
       (MonadDBRead m, MonadUnliftIO m)
    => [HDPassphrase]
    -> m [[(Address, [Word32])]]
discoverHDAddresses walletPassphrases =
    runConduitRes $ mapOutput outAddr utxoSource .| CL.fold step initWallets
  where
    initWallets = replicate (length walletPassphrases) []
    utxoSource = dbIterSource GStateDB (Proxy @UtxoIter)
    outAddr = txOutAddress . toaOut . snd

    hdPayload :: Address -> Maybe HDAddressPayload
    hdPayload (addrAttributesUnwrapped -> AddrAttributes {..}) =
        aaPkDerivationPath

    appendMaybe :: (Maybe a, b, [(b, a)]) -> [(b, a)]
    appendMaybe (Nothing, _, r) = r
    appendMaybe (Just x, b, r)  = (b, x):r

    step res address
        | Just payload <- hdPayload address = do
            let unpackResults = map (flip unpackHDAddressAttr payload) walletPassphrases
            map appendMaybe $ zip3 unpackResults (repeat address) res
        | otherwise = res
