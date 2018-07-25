module Cardano.Wallet.WalletLayer.Kernel.Accounts (
    createAccount
  , getAccount
  , deleteAccount
  , updateAccount
  ) where

import           Universum

import           Control.Lens (to)
import           Data.Acid (update)
import           Data.Coerce (coerce)
import           Data.Time.Units (Second)
import           Formatting (build, sformat)

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel

import           Cardano.Wallet.Kernel.DB.AcidState (DeleteHdAccount (..),
                     UpdateHdAccountName (..))
import           Cardano.Wallet.Kernel.DB.BlockMeta (addressMetaIsChange,
                     addressMetaIsUsed)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Read (readHdAccount)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.Kernel.Internal as Internal
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Types (CreateAccountError (..),
                     DeleteAccountError (..), GetAccountError (..),
                     UpdateAccountError (..))

import           Pos.Core (decodeTextAddress)
import qualified Pos.Core as Core

import qualified Cardano.Wallet.API.V1.Types as V1
import           Pos.Crypto.Signing

import           Cardano.Wallet.API.V1.Types (V1 (..))

createAccount :: MonadIO m
              => Kernel.PassiveWallet
              -> V1.WalletId
              -> V1.NewAccount
              -> m (Either CreateAccountError V1.Account)
createAccount wallet (V1.WalletId wId) (V1.NewAccount mbSpendingPassword accountName) = do
    liftIO $ limitExecutionTimeTo (30 :: Second) CreateAccountTimeLimitReached $ do
        case decodeTextAddress wId of
             Left _ ->
                 return $ Left (CreateAccountWalletIdDecodingFailed wId)
             Right rootAddr -> do
                let hdRootId = HD.HdRootId . InDb $ rootAddr
                let passPhrase = maybe mempty coerce mbSpendingPassword
                res <- liftIO $ Kernel.createAccount passPhrase
                                                     (HD.AccountName accountName)
                                                     (WalletIdHdRnd hdRootId)
                                                     wallet
                case res of
                     Right newAccount -> do
                         -- Create a new address to go in tandem
                         -- with this brand-new 'Account'.
                         let accountId = newAccount ^. HD.hdAccountId
                         newAddrE <- Kernel.createAddress passPhrase
                                                          (AccountIdHdRnd accountId)
                                                          wallet
                         return $ case newAddrE of
                              Left e -> Left $ CreateAccountFirstAddressGenerationFailed e
                              Right addr ->
                                  Right V1.Account {
                                      accIndex     = accountId ^. HD.hdAccountIdIx
                                                                . to HD.getHdAccountIx
                                    , accAddresses = [
                                        V1.WalletAddress {
                                               addrId            = V1.V1 addr
                                             , addrUsed          = False
                                             , addrChangeAddress = False
                                            }
                                    ]
                                    , accAmount    = V1.V1 (Core.mkCoin 0)
                                    , accName      = accountName
                                    , accWalletId  = V1.WalletId wId
                                    }
                     Left  err        -> return (Left $ CreateAccountError err)


getAccount :: MonadIO m
           => Kernel.DB
           -> V1.WalletId
           -> V1.AccountIndex
           -> m (Either GetAccountError V1.Account)
getAccount snapshot (V1.WalletId wId) accountIndex = do
    case decodeTextAddress wId of
         Left _ ->
             return $ Left (GetAccountWalletIdDecodingFailed wId)
         Right rootAddr -> do
            let hdRootId = HD.HdRootId . InDb $ rootAddr
                hdAccountId = HD.HdAccountId hdRootId (HD.HdAccountIx accountIndex)
                wallets = Kernel.hdWallets snapshot

            return $ case readHdAccount hdAccountId wallets of
                 Left kernelError -> Left $ GetAccountError kernelError
                 Right acc        -> Right $ toV1Account snapshot acc

deleteAccount :: MonadIO m
              => Kernel.PassiveWallet
              -> V1.WalletId
              -> V1.AccountIndex
              -> m (Either DeleteAccountError ())
deleteAccount wallet (V1.WalletId wId) accountIndex = do
    case decodeTextAddress wId of
         Left _ ->
             return $ Left (DeleteAccountWalletIdDecodingFailed wId)
         Right rootAddr -> do
            let hdRootId = HD.HdRootId . InDb $ rootAddr
                hdAccountId = HD.HdAccountId hdRootId (HD.HdAccountIx accountIndex)
            res <- liftIO $ update (wallet ^. Internal.wallets) (DeleteHdAccount hdAccountId)
            return $ case res of
                 Left e   -> Left (DeleteAccountError e)
                 Right () -> Right ()

updateAccount :: MonadIO m
              => Kernel.PassiveWallet
              -> V1.WalletId
              -> V1.AccountIndex
              -> V1.AccountUpdate
              -> m (Either UpdateAccountError V1.Account)
updateAccount wallet (V1.WalletId wId) accountIndex (V1.AccountUpdate newAccountName) = do
    case decodeTextAddress wId of
         Left _ ->
             return $ Left (UpdateAccountWalletIdDecodingFailed wId)
         Right rootAddr -> do
            let hdRootId = HD.HdRootId . InDb $ rootAddr
                hdAccountId = HD.HdAccountId hdRootId (HD.HdAccountIx accountIndex)
                accountName = HD.AccountName newAccountName
            res <- liftIO $ update (wallet ^. Internal.wallets) (UpdateHdAccountName hdAccountId accountName)
            return $ case res of
                 Left e                -> Left (UpdateAccountError e)
                 Right (snapshot, acc) -> Right $ toV1Account snapshot acc

{-----------------------------------------------------------------------------
    Internal utility functions
------------------------------------------------------------------------------}

-- | Converts a Kernel 'HdAccount' into a V1 'Account'.
toV1Account :: Kernel.DB -> HD.HdAccount -> V1.Account
toV1Account snapshot account =
    -- NOTE(adn): Perhaps we want the minimum or expected balance here?
    let accountAvailableBalance = Kernel.accountAvailableBalance snapshot hdAccountId
        hdAccountId  = account ^. HD.hdAccountId
        accountIndex = account ^. HD.hdAccountId . HD.hdAccountIdIx . to HD.getHdAccountIx
        hdAddresses  = Kernel.accountAddresses snapshot hdAccountId
        addresses    = IxSet.toList hdAddresses
        hdRootId     = account ^. HD.hdAccountId . HD.hdAccountIdParent
    in V1.Account {
         accIndex     = accountIndex
       , accAddresses = map (toWalletAddress snapshot) addresses
       , accAmount    = V1 accountAvailableBalance
       , accName      = account ^. HD.hdAccountName . to HD.getAccountName
       , accWalletId  = V1.WalletId (sformat build (hdRootId ^. to HD.getHdRootId . fromDb))
       }


-- | Converts a Kernel 'HdAddress' into a V1 'WalletAddress'.
toWalletAddress :: Kernel.DB
                -> HD.HdAddress
                -> V1.WalletAddress
toWalletAddress db hdAddress =
    let cardanoAddress = hdAddress ^. HD.hdAddressAddress . fromDb
        hdAccountId = hdAddress ^. HD.hdAddressId . HD.hdAddressIdParent
    in case Kernel.lookupAddressMeta db hdAccountId cardanoAddress of
           Nothing -> V1.WalletAddress (V1 cardanoAddress) False False
           Just addressMeta ->
               V1.WalletAddress (V1 cardanoAddress)
                                (addressMeta ^. addressMetaIsUsed)
                                (addressMeta ^. addressMetaIsChange)
