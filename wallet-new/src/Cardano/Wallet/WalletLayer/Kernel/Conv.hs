-- | Convert to and from V1 types
module Cardano.Wallet.WalletLayer.Kernel.Conv (
    -- * From V1 to kernel types
    fromRootId
  , fromAccountId
  , fromAssuranceLevel
    -- * From kernel types to V1 types
  , toAccountId
  , toRootId
  , toAccount
  , toWallet
  , toAddress
  , toAssuranceLevel
    -- * Convenience re-exports
  , runExcept
  , runExceptT
  , withExceptT
  , exceptT
  ) where

import           Universum

import           Control.Lens (to)
import           Control.Monad.Except
import           Formatting (build, sformat)

import           Pos.Core (decodeTextAddress)

import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.DB.BlockMeta (addressMetaIsChange,
                     addressMetaIsUsed)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (cpAddressMeta)
import           Cardano.Wallet.Kernel.DB.Spec.Read
import           Cardano.Wallet.Kernel.DB.Util.IxSet (ixedIndexed)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Util (exceptT)

{-------------------------------------------------------------------------------
  From V1 to kernel types

  Nomenclature based on kernel names, not V1 names.
-------------------------------------------------------------------------------}

fromRootId :: Monad m => V1.WalletId -> ExceptT Text m HD.HdRootId
fromRootId (V1.WalletId wId) =
    aux <$> exceptT (decodeTextAddress wId)
  where
    aux :: V1.Address -> HD.HdRootId
    aux = HD.HdRootId . InDb

fromAccountId :: Monad m
              => V1.WalletId -> V1.AccountIndex -> ExceptT Text m HD.HdAccountId
fromAccountId wId accIx =
    aux <$> fromRootId wId
  where
    aux :: HD.HdRootId -> HD.HdAccountId
    aux hdRootId = HD.HdAccountId hdRootId (HD.HdAccountIx $ V1.getAccIndex accIx)

-- | Converts from the @V1@ 'AssuranceLevel' to the HD one.
fromAssuranceLevel :: V1.AssuranceLevel -> HD.AssuranceLevel
fromAssuranceLevel V1.NormalAssurance = HD.AssuranceLevelNormal
fromAssuranceLevel V1.StrictAssurance = HD.AssuranceLevelStrict

{-------------------------------------------------------------------------------
  From kernel to V1 types
-------------------------------------------------------------------------------}

toAccountId :: HD.HdAccountId -> V1.AccountIndex
toAccountId =
    either (error . show) identity -- Invariant: Assuming HD AccountId are valid~
    . V1.mkAccountIndex
    . HD.getHdAccountIx
    . view HD.hdAccountIdIx

toRootId :: HD.HdRootId -> V1.WalletId
toRootId = V1.WalletId . sformat build . _fromDb . HD.getHdRootId

-- | Converts a Kernel 'HdAccount' into a V1 'Account'.
--
toAccount :: Kernel.DB -> HD.HdAccount -> V1.Account
toAccount snapshot account = V1.Account {
      accIndex     = accountIndex
    , accAddresses = map (toAddress account . view ixedIndexed) addresses
    , accAmount    = V1 accountAvailableBalance
    , accName      = account ^. HD.hdAccountName . to HD.getAccountName
    , accWalletId  = V1.WalletId (sformat build (hdRootId ^. to HD.getHdRootId . fromDb))
    }
  where
    -- NOTE(adn): Perhaps we want the minimum or expected balance here?
    accountAvailableBalance = cpAvailableBalance (account ^. HD.hdAccountState . HD.hdAccountStateCurrent)
    hdAccountId  = account ^. HD.hdAccountId
    accountIndex = toAccountId (account ^. HD.hdAccountId)
    hdAddresses  = Kernel.accountAddresses snapshot hdAccountId
    addresses    = IxSet.toList hdAddresses
    hdRootId     = account ^. HD.hdAccountId . HD.hdAccountIdParent

-- | Converts an 'HdRoot' into a V1 'Wallet.
toWallet :: Kernel.DB -> HD.HdRoot -> V1.Wallet
toWallet db hdRoot = V1.Wallet {
      walId                         = (V1.WalletId walletId)
    , walName                       = hdRoot ^. HD.hdRootName
                                              . to HD.getWalletName
    , walBalance                    = V1 (Kernel.rootTotalBalance db rootId)
    , walHasSpendingPassword        = hasSpendingPassword
    , walSpendingPasswordLastUpdate = V1 lastUpdate
    , walCreatedAt                  = V1 createdAt
    , walAssuranceLevel             = v1AssuranceLevel
    -- FIXME(adn) Do this as part of CBR-243.
    , walSyncState                  = V1.Synced
    }
  where
    (hasSpendingPassword, mbLastUpdate) =
        case hdRoot ^. HD.hdRootHasPassword of
             HD.NoSpendingPassword     -> (False, Nothing)
             HD.HasSpendingPassword lu -> (True, Just (lu ^. fromDb))
    -- In case the wallet has no spending password, its last update
    -- matches this wallet creation time.
    rootId           = hdRoot ^. HD.hdRootId
    createdAt        = hdRoot ^. HD.hdRootCreatedAt . fromDb
    lastUpdate       = fromMaybe createdAt mbLastUpdate
    walletId         = sformat build . _fromDb . HD.getHdRootId $ rootId
    v1AssuranceLevel = toAssuranceLevel $ hdRoot ^. HD.hdRootAssurance

toAssuranceLevel :: HD.AssuranceLevel -> V1.AssuranceLevel
toAssuranceLevel HD.AssuranceLevelNormal = V1.NormalAssurance
toAssuranceLevel HD.AssuranceLevelStrict = V1.StrictAssurance

-- | Converts a Kernel 'HdAddress' into a V1 'WalletAddress'.
toAddress :: HD.HdAccount -> HD.HdAddress -> V1.WalletAddress
toAddress acc hdAddress =
    V1.WalletAddress (V1 cardanoAddress)
                     (addressMeta ^. addressMetaIsUsed)
                     (addressMeta ^. addressMetaIsChange)
  where
    cardanoAddress = hdAddress ^. HD.hdAddressAddress . fromDb
    addressMeta    = acc ^. HD.hdAccountState . HD.hdAccountStateCurrent . cpAddressMeta cardanoAddress
