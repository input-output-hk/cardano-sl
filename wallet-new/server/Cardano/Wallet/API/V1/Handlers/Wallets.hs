module Cardano.Wallet.API.V1.Handlers.Wallets where

import           Universum

import           Cardano.Wallet.API.V1.Types
import qualified Cardano.Wallet.API.V1.Wallets as Wallets

import           Servant
import           Test.QuickCheck               (arbitrary, generate, listOf1)

handlers :: Server Wallets.API
handlers = newAccount

-- | This is an example of how POST requests might look like. The user would be
-- required to submit the whole `Account`, with all the non-Maybe fields properly
-- populated, and the backend will simply ignore things which are not for user
-- to specify. For an `Account`, obvious choices are the amount of coins.
newAccount :: WalletId -> Account -> Handler Account
newAccount wId Account{..} = do
    -- In real code we would generate things like addresses (if needed) or
    -- any other form of Id/data.
    accId <- liftIO $ generate (listOf1 arbitrary)
    return $ Account {
             acc_id = fromString accId
           , acc_amount = 0
           , acc_addresses = mempty
           , acc_name = acc_name
           , acc_wallet_id = wId
           }
