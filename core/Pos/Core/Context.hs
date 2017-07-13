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

import qualified Ether

import           Pos.Core.Address (addressHash, makePubKeyAddress)
import           Pos.Core.Types   (Address, StakeholderId)
import           Pos.Crypto       (PublicKey, SecretKey, toPublic)

data PrimaryKeyTag

-- | Access to primary key of the node.
type MonadPrimaryKey = Ether.MonadReader PrimaryKeyTag SecretKey

getOurSecretKey :: MonadPrimaryKey m => m SecretKey
getOurSecretKey = Ether.ask @PrimaryKeyTag

getOurPublicKey :: MonadPrimaryKey m => m PublicKey
getOurPublicKey = toPublic <$> getOurSecretKey

getOurKeys :: MonadPrimaryKey m => m (SecretKey, PublicKey)
getOurKeys = (identity &&& toPublic) <$> getOurSecretKey

getOurStakeholderId :: MonadPrimaryKey m => m StakeholderId
getOurStakeholderId = addressHash <$> getOurPublicKey

getOurPubKeyAddress :: MonadPrimaryKey m => m Address
getOurPubKeyAddress = makePubKeyAddress <$> getOurPublicKey
