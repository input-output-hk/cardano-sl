{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE StrictData    #-}

-- | This module defines a client interface for the Cardano wallet.
module Cardano.Wallet.Client
    ( -- * The abstract client
      WalletClient(..)
    , getWalletIndex
    , getAccounts
    , getWallets
    , getAddressIndex
    , getTransactionIndex
    , Resp
    , hoistClient
    , liftClient
    -- * The type of errors that the client might return
    , ClientError(..)
    , V1Errors.WalletError(..)
    , ServantError(..)
    , Response(..)
    -- * Reexports
    , module Cardano.Wallet.API.V1.Types
    , module Cardano.Wallet.API.V1.Parameters
    , module Cardano.Wallet.API.Request.Pagination
    , FilterOperations(..)
    , SortOperations(..)
    , FilterOperation(..)
    , SortOperation(..)
    , WalletResponse(..)
    ) where

import           Universum

import           Control.Exception (Exception (..))
import           Servant.Client (Response (..), ServantError (..))

import           Cardano.Wallet.API.Request.Filter
import           Cardano.Wallet.API.Request.Pagination
import           Cardano.Wallet.API.Request.Sort
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Errors as V1Errors
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types
import qualified Pos.Core as Core

-- | A representation of a wallet client parameterized over some effect
-- type @m@.
--
-- The record fields serve as the API to the wallet client. Note that the
-- field 'getAddressIndex' has the type:
--
-- @
-- 'getAddressIndex'
--     :: 'WalletClient' m
--     -> m ('Either' 'WalletError' ('WalletResponse' ['Address']))
-- @
--
-- Other functions may be defined in terms of this 'WalletClient' -- see
-- 'getWalletIndex' as a convenience helper for 'getWalletIndexPaged'.
-- TODO(ks): I don't think that it's important to preserve paging as
-- an important detail, we should remove paging and return the full set
-- of results.
data WalletClient m
    = WalletClient
    { -- address endpoints
      getAddressIndexPaginated
         :: Maybe Page -> Maybe PerPage -> Resp m [WalletAddress]
    , postAddress
         :: NewAddress -> Resp m WalletAddress
    , getAddress
         :: Text -> Resp m WalletAddress
    -- wallets endpoints
    , postWallet
         :: New Wallet -> Resp m Wallet
    , getWalletIndexFilterSorts
         :: Maybe Page
         -> Maybe PerPage
         -> FilterOperations Wallet
         -> SortOperations Wallet
         -> Resp m [Wallet]
    , updateWalletPassword
         :: WalletId -> PasswordUpdate -> Resp m Wallet
    , deleteWallet
         :: WalletId -> m (Either ClientError ())
    , getWallet
        :: WalletId -> Resp m Wallet
    , updateWallet
         :: WalletId -> Update Wallet -> Resp m Wallet
    -- account endpoints
    , deleteAccount
         :: WalletId -> AccountIndex -> m (Either ClientError ())
    , getAccount
         :: WalletId -> AccountIndex -> Resp m Account
    , getAccountIndexPaged
         :: WalletId -> Maybe Page -> Maybe PerPage -> Resp m [Account]
    , postAccount
        :: WalletId -> New Account -> Resp m Account
    , updateAccount
         :: WalletId -> AccountIndex -> Update Account -> Resp m Account
    -- transactions endpoints
    , postTransaction
         :: Payment -> Resp m Transaction
    , getTransactionIndexFilterSorts
         :: Maybe WalletId
         -> Maybe AccountIndex
         -> Maybe (V1 Core.Address)
         -> Maybe Page
         -> Maybe PerPage
         -> FilterOperations Transaction
         -> SortOperations Transaction
         -> Resp m [Transaction]
    , getTransactionFee
         :: Payment -> Resp m EstimatedFees
    -- settings
    , getNodeSettings
         :: Resp m NodeSettings
    -- info
    , getNodeInfo
         :: Resp m NodeInfo
    } deriving Generic

-- | Paginates through all request pages and concatenates the result.
--
-- NOTE: this lazy variant might be inefficient. It is supposed to be used only in tests. Implement strict version if optimization is needed
-- TODO(akegalj): this can be paralelized like so (pseudo):
--   do
--     -- first page is fetched in sequence
--     page1 <- request (page 1) (Just maxPerPageEntries)
--     -- then rest of the pages is fetched in parallel
--     fromPage2 <- paralelMap (\p -> request p $ Just 50) [2..page1.wrMeta.metaTotalPages]
--     concatMap wrData $ page1:fromPage2
--
paginateAll :: Monad m => (Maybe Page -> Maybe PerPage -> Resp m [a]) -> Resp m [a]
paginateAll request = fmap fixMetadata <$> paginatePage 1
  where
    fixMetadata WalletResponse{..} =
        WalletResponse
            { wrMeta = Metadata $
                PaginationMetadata
                    { metaTotalPages = 1
                    , metaPage = Page 1
                    , metaPerPage = PerPage $ length wrData
                    , metaTotalEntries = length wrData
                    }
            , ..
            }
    paginatePage page = do
        result <- request (Just $ Page page) (Just $ PerPage maxPerPageEntries)
        case result of
            Left _ -> pure result
            Right resp ->
                if null $ wrData resp
                    then pure result
                    else fmap (\d -> d { wrData = wrData resp <> wrData d }) <$> paginatePage (succ page)

getAddressIndex :: Monad m => WalletClient m -> Resp m [WalletAddress]
getAddressIndex = paginateAll . getAddressIndexPaginated

getAccounts :: Monad m => WalletClient m -> WalletId -> Resp m [Account]
getAccounts wc = paginateAll . getAccountIndexPaged wc

getTransactionIndex
    :: Monad m
    => WalletClient m
    -> Maybe WalletId
    -> Maybe AccountIndex
    -> Maybe (V1 Core.Address)
    -> Resp m [Transaction]
getTransactionIndex wc wid maid maddr =
    paginateAll $ \mp mpp -> getTransactionIndexFilterSorts wc wid maid maddr mp mpp NoFilters NoSorts

getWalletIndexPaged :: WalletClient m -> Maybe Page -> Maybe PerPage -> Resp m [Wallet]
getWalletIndexPaged wc mp mpp = getWalletIndexFilterSorts wc mp mpp NoFilters NoSorts

-- | Retrieves only the first page of wallets, providing a default value to
-- 'Page' and 'PerPage'.
getWallets :: Monad m => WalletClient m -> Resp m [Wallet]
getWallets = paginateAll . getWalletIndexPaged

-- | Run the given natural transformation over the 'WalletClient'.
hoistClient :: (forall x. m x -> n x) -> WalletClient m -> WalletClient n
hoistClient phi wc = WalletClient
    { getAddressIndexPaginated =
         \x -> phi . getAddressIndexPaginated wc x
    , postAddress =
         phi . postAddress wc
    , getAddress =
         phi . getAddress wc
    , postWallet =
         phi . postWallet wc
    , getWalletIndexFilterSorts =
         \x y p -> phi . getWalletIndexFilterSorts wc x y p
    , updateWalletPassword =
         \x -> phi . updateWalletPassword wc x
    , deleteWallet =
         phi . deleteWallet wc
    , getWallet =
         phi . getWallet wc
    , updateWallet =
         \x -> phi . updateWallet wc x
    , deleteAccount =
         \x -> phi . deleteAccount wc x
    , getAccount =
         \x -> phi . getAccount wc x
    , getAccountIndexPaged =
         \x mp -> phi . getAccountIndexPaged wc x mp
    , postAccount =
         \x -> phi . postAccount wc x
    , updateAccount =
         \x y -> phi . updateAccount wc x y
    , postTransaction =
         phi . postTransaction wc
    , getTransactionIndexFilterSorts =
         \wid maid maddr mp mpp f ->
             phi . getTransactionIndexFilterSorts wc wid maid maddr mp mpp f
    , getTransactionFee =
         phi . getTransactionFee wc
    , getNodeSettings =
         phi (getNodeSettings wc)
    , getNodeInfo =
         phi (getNodeInfo wc)
    }

-- | Generalize a @'WalletClient' 'IO'@ into a @('MonadIO' m) =>
-- 'WalletClient' m@.
liftClient :: MonadIO m => WalletClient IO -> WalletClient m
liftClient = hoistClient liftIO

-- | Calls 'getWalletIndexPaged' using the 'Default' values for 'Page' and
-- 'PerPage'.
getWalletIndex :: Monad m => WalletClient m -> Resp m [Wallet]
getWalletIndex = paginateAll . getWalletIndexPaged

-- | A type alias shorthand for the response from the 'WalletClient'.
type Resp m a = m (Either ClientError (WalletResponse a))

-- | The type of errors that the wallet might return.
data ClientError
    = ClientWalletError V1Errors.WalletError
    -- ^ The 'WalletError' type represents known failures that the API
    -- might return.
    | ClientHttpError ServantError
    -- ^ We directly expose the 'ServantError' type as part of this
    | UnknownClientError SomeException
    -- ^ This constructor is used when the API client reports an error that
    -- isn't represented in either the 'ServantError' HTTP errors or the
    -- 'WalletError' for API errors.
    deriving (Show, Generic)

-- | General (and naive) equality instance.
instance Eq ClientError where
    ClientWalletError  e1 == ClientWalletError  e2 = e1 == e2
    ClientHttpError    e1 == ClientHttpError    e2 = e1 == e2
    UnknownClientError _  == UnknownClientError _  = True
    _ == _ = False

-- | General exception instance.
instance Exception ClientError where
    toException (ClientWalletError  e) = toException e
    toException (ClientHttpError    e) = toException e
    toException (UnknownClientError e) = toException e

