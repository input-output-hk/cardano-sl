{-# LANGUAGE TypeFamilies #-}

-- | Access to all secrets in the system. It's needed mostly for
-- testing. Not for production.

module Pos.AllSecrets
       ( InvSecretsMap
       , unInvSecretsMap
       , mkInvSecretsMap
       , InvAddrSpendingData
       , unInvAddrSpendingData
       , mkInvAddrSpendingData
       , AllSecrets (..)
       , HasAllSecrets (..)
       , mkAllSecretsSimple
       ) where

import           Universum

import           Control.Lens.TH (makeClassy)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable
import           Formatting (bprint, int, (%))
import           Serokell.Util (listJson, mapJson)

import           Pos.Binary.Core ()
import           Pos.Core (AddrSpendingData (..), Address, IsBootstrapEraAddr (..), StakeholderId,
                           addressHash, checkAddrSpendingData, makePubKeyAddress,
                           makePubKeyAddressBoot)
import           Pos.Crypto (PublicKey, SecretKey, toPublic)

-- | This map effectively provides inverse of 'hash' and
-- 'toPublic' functions. It's quite useful in tests and block
-- generator. (/Inv/ means /inverse/).
newtype InvSecretsMap = InvSecretsMap
    { unInvSecretsMap :: HashMap StakeholderId SecretKey
    } deriving (Semigroup, Monoid, Container)

-- | Make 'InvSecretsMap' from a list of secret keys.
mkInvSecretsMap :: [SecretKey] -> InvSecretsMap
mkInvSecretsMap =
    let toSecretPair sk = (addressHash (toPublic sk), sk)
    in InvSecretsMap . HM.fromList . map toSecretPair

-- | This map allows to get 'AddrSpendingData' corresponding to an
-- 'Address'.
newtype InvAddrSpendingData = InvAddrSpendingData
    { unInvAddrSpendingData :: HashMap Address AddrSpendingData
    } deriving (Semigroup, Monoid, Container)

-- | Safe constructor of 'InvAddrSpendingData'.
mkInvAddrSpendingData :: [(Address, AddrSpendingData)] -> InvAddrSpendingData
mkInvAddrSpendingData pairs
    | check pairs = InvAddrSpendingData (HM.fromList pairs)
    | otherwise = error "mkInvAddrSpendingData: check failed"
  where
    check = all checkPair
    checkPair :: (Address, AddrSpendingData) -> Bool
    checkPair (addr, asd) = checkAddrSpendingData asd addr

-- | All secrets in the system.
--
-- TODO: probably VSS keys should be added here at some point.
data AllSecrets = AllSecrets
    { _asSecretKeys   :: !InvSecretsMap
    -- ^ Secret keys of all stakeholders participating in the system.
    , _asSpendingData :: !InvAddrSpendingData
    -- ^ Inverse of 'addressHash @AddrSpendingData'.
    }

makeClassy ''AllSecrets

instance Buildable AllSecrets where
    build AllSecrets {..} =
        bprint ("AllSecrets {\n"%
                "  secret keys: "%int%" items\n"%
                "  stakeholders: "%listJson%"\n"%
                "  inverse spending data: "%mapJson%"\n"%
                "}\n")
            (length _asSecretKeys)
            (HM.keys $ unInvSecretsMap _asSecretKeys)
            (unInvAddrSpendingData _asSpendingData)

-- | Make simple 'AllSecrets' assuming that only public key addresses
-- with bootstrap era distribution and single key distribution exist
-- in the system.
mkAllSecretsSimple :: [SecretKey] -> AllSecrets
mkAllSecretsSimple sks =
    AllSecrets
    { _asSecretKeys = mkInvSecretsMap sks
    , _asSpendingData = invAddrSpendingData
    }
  where
    pks :: [PublicKey]
    pks = map toPublic sks
    spendingDataList = map PubKeyASD pks
    addressesNonBoot = map (makePubKeyAddress (IsBootstrapEraAddr False)) pks
    addressesBoot = map makePubKeyAddressBoot pks
    invAddrSpendingData =
        mkInvAddrSpendingData $
        zip addressesNonBoot spendingDataList <>
        zip addressesBoot spendingDataList
