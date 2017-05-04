-- | This module contains functions, which iterate over Utxo and receive
-- addresses which correspond to specified HD passphrases.

module Pos.Crypto.HDDiscovery
       ( discoverHDAddress
       , discoverHDAddresses
       ) where

import           Universum

import           Pos.Core.Types      (Address (..), addrPkDerivationPath)
import           Pos.Crypto.HD       (HDAddressPayload, HDPassphrase, unpackHDAddressAttr)
import           Pos.Data.Attributes (attrData)
import           Pos.DB.Class        (MonadDB)
import           Pos.Txp.Core        (toaOut, txOutAddress)
import           Pos.Txp.DB          (UtxoIter, runUtxoMapIterator)
import           Pos.Util.Iterator   (MonadIterator (..))

discoverHDAddress :: MonadDB m => HDPassphrase -> m [(Address, [Word32])]
discoverHDAddress walletPassphrase = safeHead <$> discoverHDAddresses [walletPassphrase]
  where
    safeHead [x] = x
    safeHead _ = []

discoverHDAddresses :: MonadDB m => [HDPassphrase] -> m [[(Address, [Word32])]]
discoverHDAddresses walletPassphrases =
    runUtxoMapIterator @UtxoIter (step $ replicate (length walletPassphrases) [])
                                 (txOutAddress . toaOut . snd)
  where
    hdPayload :: Address -> Maybe HDAddressPayload
    hdPayload (PubKeyAddress _ p) =
        addrPkDerivationPath . attrData $ p
    hdPayload _ = Nothing

    onItem !res address
        | Just payload <- hdPayload address = do
            let unpackResults = map (flip unpackHDAddressAttr payload) walletPassphrases
            step $ map appendMaybe $ zip3 unpackResults (repeat address) res
        | otherwise = step res
    step res = nextItem >>= maybe (pure res) (onItem res)

    appendMaybe :: (Maybe a, b, [(b, a)]) -> [(b, a)]
    appendMaybe (Nothing, _, r) = r
    appendMaybe (Just x, b, r) = (b, x):r
