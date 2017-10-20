module Cardano.Wallet.API.V1.Handlers.Wallets where

import           Universum

import           Cardano.Wallet.API.V1.Errors  as Errors
import           Cardano.Wallet.API.V1.Types
import qualified Cardano.Wallet.API.V1.Wallets as Wallets

import           Data.Aeson                    (encode)
import qualified Network.HTTP.Types.Header     as HTTP
import           Servant
import           Test.QuickCheck               (arbitrary, generate, listOf1, resize)

applicationJson :: HTTP.Header
applicationJson =
    let [hdr] = getHeaders (addHeader "application/json" mempty :: (Headers '[Header "Content-Type" String] String))
    in hdr

toError :: ServantErr -> WalletError -> ServantErr
toError err@ServantErr{..} we =
    err { errBody = encode we
        , errHeaders = applicationJson : errHeaders
        }

handlers :: Server Wallets.API
handlers =   newWallet
        :<|> listWallets
        :<|> newAccount

newWallet :: Wallet -> Handler Wallet
newWallet = return . identity

listWallets :: Maybe Page
            -> Maybe PerPage
            -> Maybe Bool
            -> Maybe Text
            -> Handler (OneOf [Wallet] (ExtendedResponse [Wallet]))
listWallets _ _ mbExtended _ = do
  example <- liftIO $ generate (resize 3 arbitrary)
  case mbExtended of
    Just True  -> return $ OneOf $ Right $
      ExtendedResponse {
        ext_data = example
      , ext_meta = Metadata {
          meta_total_pages = 1
        , meta_page = 1
        , meta_per_page = 20
        , meta_total_entries = 3
      }
      }
    _ -> return $ OneOf $ Left example

-- | This is an example of how POST requests might look like. The user would be
-- required to submit the whole `Account`, with all the non-Maybe fields properly
-- populated, and the backend will simply ignore things which are not for user
-- to specify. For an `Account`, obvious choices are the amount of coins.
--
-- It also shows an example of how an error might look like.
-- NOTE: This will probably change drastically as soon as we start using our
-- custom monad as a base of the Handler stack, so the example here is just to
-- give the idea of how it will look like on Swagger.
newAccount :: WalletId -> Maybe Text -> Account -> Handler Account
newAccount w@(WalletId wId) _ Account{..} = do
    when (wId /= "testwallet") $ throwError (toError err404 Errors.walletNotFound)
    -- In real code we would generate things like addresses (if needed) or
    -- any other form of Id/data.
    accId <- liftIO $ generate (listOf1 arbitrary)
    return $ Account {
             acc_id = fromString accId
           , acc_amount = 0
           , acc_addresses = mempty
           , acc_name = acc_name
           , acc_walletId = w
           }
