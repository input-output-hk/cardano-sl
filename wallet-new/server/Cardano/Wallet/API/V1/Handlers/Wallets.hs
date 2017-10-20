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
        extData = example
      , extMeta = Metadata {
          metaTotalPages = 1
        , metaPage = 1
        , metaPerPage = 20
        , metaTotalEntries = 3
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
    newId <- liftIO $ generate (listOf1 arbitrary)
    return $ Account {
             accId = fromString newId
           , accAmount = 0
           , accAddresses = mempty
           , accName = accName
           , accWalletId = w
           }
