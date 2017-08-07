-- | Core runtime context.

module Pos.Core.Context
       ( CoreConstants (..)
       , ccBlkSecurityParam
       , blkSecurityParam

       , HasPrimaryKey(..)
       , getOurSecretKey
       , getOurPublicKey
       , getOurKeys
       , getOurStakeholderId
       , getOurPubKeyAddress
       ) where

import           Universum

import           Control.Lens     (Getter, makeLenses)
import           Data.Reflection  (Given (..))

import           Pos.Core.Address (addressHash, makePubKeyAddress)
import           Pos.Core.Types   (Address, BlockCount, StakeholderId)
import           Pos.Crypto       (PublicKey, SecretKey, toPublic)

-- | Core constants. They should be really constant and never change.
data CoreConstants = CoreConstants
    { _ccBlkSecurityParam :: !BlockCount
    }

makeLenses ''CoreConstants

blkSecurityParam :: Given CoreConstants => BlockCount
blkSecurityParam = _ccBlkSecurityParam given

-- | Access to primary key of the node.
class HasPrimaryKey ctx where
    primaryKey :: Lens' ctx SecretKey

getOurSecretKey :: (MonadReader ctx m, HasPrimaryKey ctx) => m SecretKey
getOurSecretKey = view primaryKey

getOurPublicKey :: (MonadReader ctx m, HasPrimaryKey ctx) => m PublicKey
getOurPublicKey = toPublic <$> getOurSecretKey

getOurKeys :: (MonadReader ctx m, HasPrimaryKey ctx) => m (SecretKey, PublicKey)
getOurKeys = (identity &&& toPublic) <$> getOurSecretKey

getOurStakeholderId :: (MonadReader ctx m, HasPrimaryKey ctx) => m StakeholderId
getOurStakeholderId = addressHash <$> getOurPublicKey

getOurPubKeyAddress :: (MonadReader ctx m, HasPrimaryKey ctx) => m Address
getOurPubKeyAddress = makePubKeyAddress <$> getOurPublicKey
