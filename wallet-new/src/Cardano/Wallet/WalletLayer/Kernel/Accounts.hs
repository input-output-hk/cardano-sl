module Cardano.Wallet.WalletLayer.Kernel.Accounts (
    createAccount
  , getAccount
  , getAccountBalance
  , getAccountAddresses
  , getAccounts
  , deleteAccount
  , updateAccount
  ) where

import           Universum

import           Data.Coerce (coerce)

import qualified Pos.Core as Core

import           Cardano.Wallet.API.Request (RequestParams, SortOperations (..))
import           Cardano.Wallet.API.Request.Filter (FilterOperations (..))
import           Cardano.Wallet.API.Response (WalletResponse, respondWith)
import           Cardano.Wallet.API.V1.Types (V1 (..), WalletAddress)
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.Read (addressesByAccountId)
import           Cardano.Wallet.Kernel.DB.Util.IxSet (Indexed (..), IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.WalletLayer (CreateAccountError (..),
                     DeleteAccountError (..), GetAccountError (..),
                     GetAccountsError (..), UpdateAccountError (..))
import           Cardano.Wallet.WalletLayer.Kernel.Conv

-- | Creates a new account. It does @not@ create a default address to go
-- alongside this wallet nor it should be, as the invariant applies only to
-- new wallet being created/restored and is enforced elsewhere.
createAccount :: MonadIO m
              => Kernel.PassiveWallet
              -> V1.WalletId
              -> V1.NewAccount
              -> m (Either CreateAccountError V1.Account)
createAccount wallet wId (V1.NewAccount _ accountName) = liftIO $ runExceptT $  do
    rootId <- withExceptT CreateAccountWalletIdDecodingFailed $
                fromRootId wId
    (db, acc) <- withExceptT CreateAccountError $ ExceptT $ liftIO $
                     Kernel.createAccount (HD.AccountName accountName)
                                          (WalletIdHdRnd rootId)
                                          wallet
    let accId = acc ^. HD.hdAccountId
    let accountAddresses = addressesByAccountId db accId
    pure $ mkAccount acc (IxSet.toList accountAddresses)
  where
    mkAccount :: HD.HdAccount -> [Indexed HD.HdAddress] -> V1.Account
    mkAccount account addresses = V1.Account {
        accIndex     = toAccountId (account ^. HD.hdAccountId)
      , accAmount    = V1.V1 (Core.mkCoin 0)
      , accName      = accountName
      , accWalletId  = wId
      , accAddresses = map (toAddress account . view IxSet.ixedIndexed) addresses
      }

-- | Retrieves a full set of accounts.
getAccounts :: V1.WalletId
            -> Kernel.DB
            -> Either GetAccountsError (IxSet V1.Account)
getAccounts wId snapshot = runExcept $ do
    rootId <- withExceptT GetAccountsWalletIdDecodingFailed $ fromRootId wId
    fmap conv $
      withExceptT GetAccountsError $ exceptT $ do
        _rootExists <- Kernel.lookupHdRootId snapshot rootId
        return $ Kernel.accountsByRootId snapshot rootId
  where
    -- NOTE(adn) [CBR-347] This has currently terrible performance due to the
    -- fact we still have to unify the 'IxSet' with the 'IxSet'. Not only that,
    -- but due to the fact we cannot map directly on an 'IxSet' (neither the
    -- kernel nor the native one) this suggests a different shape for the
    -- WalletLayer API, mainly that:
    --
    -- 1. Sorting & Filtering parameters somehow are defined not on the
    --    API-specific data types but on the Kernel types.
    -- 2. Each WalletLayer function that returns a collection should sort &
    --    filter directly on the Kernel IxSet, convert the data into its final
    --    type -- (i.e. a list) and offer it for consumption to the Servant
    --    Handlers.
    --
    -- This would also ensure maximum compatibility with the WalletLayer.Legacy
    -- and the migrated datatypes.
    conv :: IxSet HD.HdAccount -> IxSet V1.Account
    conv = IxSet.fromList . map (toAccount snapshot) . IxSet.toList

-- | Retrieves a single account.
getAccount :: V1.WalletId
           -> V1.AccountIndex
           -> Kernel.DB
           -> Either GetAccountError V1.Account
getAccount wId accIx snapshot = runExcept $ do
    accId <- withExceptT GetAccountWalletIdDecodingFailed $
               fromAccountId wId accIx
    fmap (toAccount snapshot) $
      withExceptT GetAccountError $ exceptT $
        Kernel.lookupHdAccountId snapshot accId

getAccountBalance :: V1.WalletId
                  -> V1.AccountIndex
                  -> Kernel.DB
                  -> Either GetAccountError V1.AccountBalance
getAccountBalance wId accIx snapshot = runExcept $ do
    accId <- withExceptT GetAccountWalletIdDecodingFailed $
               fromAccountId wId accIx
    fmap (V1.AccountBalance . V1) $
      withExceptT GetAccountError $ exceptT $
        Kernel.currentTotalBalance snapshot accId

deleteAccount :: MonadIO m
              => Kernel.PassiveWallet
              -> V1.WalletId
              -> V1.AccountIndex
              -> m (Either DeleteAccountError ())
deleteAccount wallet wId accIx = runExceptT $ do
    accId <- withExceptT DeleteAccountWalletIdDecodingFailed $
               fromAccountId wId accIx
    withExceptT DeleteAccountError $ ExceptT $ liftIO $
      Kernel.deleteAccount accId wallet

updateAccount :: MonadIO m
              => Kernel.PassiveWallet
              -> V1.WalletId
              -> V1.AccountIndex
              -> V1.AccountUpdate
              -> m (Either UpdateAccountError V1.Account)
updateAccount wallet wId accIx (V1.AccountUpdate newName) = runExceptT $ do
    accId <- withExceptT UpdateAccountWalletIdDecodingFailed $
               fromAccountId wId accIx
    fmap (uncurry toAccount) $
      withExceptT UpdateAccountError $ ExceptT $ liftIO $
        Kernel.updateAccount accId (HD.AccountName newName) wallet

getAccountAddresses :: V1.WalletId
                    -> V1.AccountIndex
                    -> RequestParams
                    -> FilterOperations '[V1 Core.Address] WalletAddress
                    -> Kernel.DB
                    -> Either GetAccountError (WalletResponse [V1.WalletAddress])
getAccountAddresses wId accIx rp fo snapshot = runExcept $ do
    accId <- withExceptT GetAccountWalletIdDecodingFailed $
               fromAccountId wId accIx
    acc   <- withExceptT GetAccountError $ exceptT $
               Kernel.lookupHdAccountId snapshot accId
    let allAddrs = Kernel.addressesByAccountId snapshot accId
    resp  <- respondWith rp (filterHdAddress fo) NoSorts $ return allAddrs
    return $ map (toAddress acc . _ixedIndexed) <$> resp

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

filterHdAddress :: FilterOperations '[V1 Core.Address] WalletAddress
                -> FilterOperations '[V1 Core.Address] (Indexed HD.HdAddress)
filterHdAddress NoFilters               = NoFilters
filterHdAddress (FilterNop NoFilters)   = FilterNop NoFilters
filterHdAddress (FilterOp op NoFilters) = FilterOp (coerce op) NoFilters
