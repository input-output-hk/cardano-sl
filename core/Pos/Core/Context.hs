{-# LANGUAGE RankNTypes  #-}

-- | Core runtime context.

module Pos.Core.Context
       ( CoreConstants (..)
       , HasCoreConstants
       , giveConsts
       , giveStaticConsts
       , ccBlkSecurityParam
       , blkSecurityParam
       , slotSecurityParam
       , epochSlots
       , epochSlotsRaw
       , chainQualityThreshold

       , HasPrimaryKey(..)
       , getOurSecretKey
       , getOurPublicKey
       , getOurKeys
       , getOurStakeholderId
       , getOurPubKeyAddress
       ) where

import           Universum

import           Control.Lens       (makeLenses)
import           Data.Reflection    (Given (..), give)

import           Pos.Core.Address   (addressHash, makePubKeyAddress)
import           Pos.Core.Types     (Address, BlockCount (..), StakeholderId, SlotCount)
import           Pos.Crypto         (PublicKey, SecretKey, toPublic)
import           Pos.Core.Constants (staticBlkSecurityParam)

-- | Core constants. They should be really constant and never change.
data CoreConstants = CoreConstants
    { _ccBlkSecurityParam :: !BlockCount
    }

makeLenses ''CoreConstants

type HasCoreConstants = Given CoreConstants

giveConsts :: BlockCount -> (HasCoreConstants => r) -> r
giveConsts = give . CoreConstants

giveStaticConsts :: (HasCoreConstants => r) -> r
giveStaticConsts = giveConsts staticBlkSecurityParam

blkSecurityParam :: HasCoreConstants => BlockCount
blkSecurityParam = _ccBlkSecurityParam given

-- | Security parameter expressed in number of slots. It uses chain
-- quality property. It's basically @blkSecurityParam / chainQualityThreshold@.
slotSecurityParam :: HasCoreConstants => SlotCount
slotSecurityParam = fromIntegral $ 2 * getBlockCount blkSecurityParam

-- We don't have a special newtype for it, so it can be any
-- 'Fractional'. I think adding newtype here would be overkill
-- (@gromak). Also this value is not actually part of the protocol,
-- but rather implementation detail, so we don't need to ensure
-- conrete precision. Apart from that, in reality we know that it's
-- 0.5, so any fractional type should be fine ☺
--
-- | Minimal chain quality (number of blocks divided by number of
-- slots) necessary for security of the system.
chainQualityThreshold :: (HasCoreConstants, Fractional fractional) => fractional
chainQualityThreshold =
    realToFrac blkSecurityParam / realToFrac slotSecurityParam

-- | Number of slots inside one epoch.
epochSlots :: HasCoreConstants => SlotCount
epochSlots = epochSlotsRaw

-- | Number of slots inside one epoch.
epochSlotsRaw :: (HasCoreConstants, Integral a) => a
epochSlotsRaw = fromIntegral $ 10 * getBlockCount blkSecurityParam

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
