module Pos.Core.Context
       ( PrimaryKeyTag
       , MonadPrimaryKey
       , getOurSecretKey
       , getOurPublicKey
       , getOurKeys
       , getOurStakeholderId
       , getOurPubKeyAddress
       ) where

import           Universum

import           EtherCompat

import           Pos.Core.Address (addressHash, makePubKeyAddress)
import           Pos.Core.Types   (Address, StakeholderId)
import           Pos.Crypto       (PublicKey, SecretKey, toPublic)

data PrimaryKeyTag

-- | Access to primary key of the node.
type MonadPrimaryKey ctx m = (MonadReader ctx m, HasLens PrimaryKeyTag ctx SecretKey)

getOurSecretKey :: MonadPrimaryKey ctx m => m SecretKey
getOurSecretKey = view (lensOf @PrimaryKeyTag)

getOurPublicKey :: MonadPrimaryKey ctx m => m PublicKey
getOurPublicKey = toPublic <$> getOurSecretKey

getOurKeys :: MonadPrimaryKey ctx m => m (SecretKey, PublicKey)
getOurKeys = (identity &&& toPublic) <$> getOurSecretKey

getOurStakeholderId :: MonadPrimaryKey ctx m => m StakeholderId
getOurStakeholderId = addressHash <$> getOurPublicKey

getOurPubKeyAddress :: MonadPrimaryKey ctx m => m Address
getOurPubKeyAddress = makePubKeyAddress <$> getOurPublicKey
