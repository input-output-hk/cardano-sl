{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.API.V1.Handlers.Transactions where

import           Universum

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.History as V0
import           Cardano.Wallet.API.V1.Migration (MonadV1, HasConfigurations, HasCompileInfo, migrate)

import qualified Cardano.Wallet.API.V1.Transactions as Transactions
import           Cardano.Wallet.API.V1.Types

import           Servant
import           Test.QuickCheck (arbitrary, generate)

handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Transactions.API MonadV1

handlers = newTransaction
        :<|> allTransactions
        :<|> estimateFees

newTransaction :: Payment -> MonadV1 Transaction
newTransaction _ = liftIO $ generate arbitrary

-- | The conclusion is that we want just the walletId for now, the details
-- in CSL-1917. We are ignoring @PaginationParams@ for now.
allTransactions
    :: (V0.MonadWalletHistory ctx m)
    => WalletId
    -> m (ExtendedResponse [Transaction])
allTransactions walletId = do
    cIdWallet    <- migrate walletId
    transactions <- V0.getHistory cIdWallet [] Nothing >>= migrate

    pure ExtendedResponse
        { extData = transactions
        , extMeta = Metadata
            { metaTotalPages = 1
            , metaPage = 1
            , metaPerPage = 20
            , metaTotalEntries = 3
            }
        }

estimateFees :: Payment -> MonadV1 EstimatedFees
estimateFees _ = liftIO $ generate arbitrary

