-- |

module Pos.Crypto.HDDiscovery
       ( discoverHDAddress
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
discoverHDAddress walletPassphrase =
    runUtxoMapIterator @UtxoIter (step []) (txOutAddress . toaOut . snd)
  where
    hdPayload :: Address -> Maybe HDAddressPayload
    hdPayload (PubKeyAddress _ p) =
        addrPkDerivationPath . attrData $ p
    hdPayload _ = Nothing

    onItem res address
        | Just path <- hdPayload address >>= unpackHDAddressAttr walletPassphrase =
            step ((address, path):res)
        | otherwise = step res
    step res = nextItem >>= maybe (pure res) (onItem res)
