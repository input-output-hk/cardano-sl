{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server
       ( walletApplication
       , walletServeWeb
       ) where

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Except     (MonadError (throwError))
import           Formatting               (ords, sformat, (%))
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant.API              ((:<|>) ((:<|>)),
                                           FromHttpApiData (parseUrlPiece))
import           Servant.Server           (Handler, ServantErr (errBody), Server, ServerT,
                                           err404, serve)
import           Servant.Utils.Enter      ((:~>) (Nat), enter)
import           System.Wlog              (LoggerNameBox, logInfo, usingLoggerName)
import           Universum

import           Pos.Crypto               (parseFullPublicKey)
import           Pos.Types                (Address, Coin (Coin), addressF, coinF,
                                           makePubKeyAddress)
import           Pos.Wallet.Web.Api       (WalletApi, walletApi)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

walletServeWeb :: MonadIO m => Word16 -> m ()
walletServeWeb = liftIO . flip run walletApplication . fromIntegral

walletApplication :: Application
walletApplication = serve walletApi servantServer

----------------------------------------------------------------------------
-- Servant infrastructure
----------------------------------------------------------------------------

type WebHandler = LoggerNameBox IO

convertHandler :: forall a . WebHandler a -> Handler a
convertHandler a =
    liftIO (usingLoggerName "wallet-web" a) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: WebHandler :~> Handler
nat = Nat convertHandler

servantServer :: Server WalletApi
servantServer = enter nat servantHandlers

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

servantHandlers :: ServerT WalletApi WebHandler
servantHandlers = getAddresses :<|> getBalances :<|> send

getAddresses :: WebHandler [Address]
getAddresses = pure []

getBalances :: WebHandler [(Address, Coin)]
getBalances = pure []

send :: Word -> Address -> Coin -> WebHandler ()
send srcIdx dstAddr c
    | srcIdx > 42 =
        throwM err404 { errBody = "There are only 42 addresses in wallet" }
    | otherwise =
        logInfo $
        sformat ("Successfully sent "%coinF%" from "%ords%" address to "%addressF)
        c srcIdx dstAddr

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance FromHttpApiData Coin

instance FromHttpApiData Address where
    parseUrlPiece = fmap makePubKeyAddress . maybe onError pure . parseFullPublicKey
      where
        onError = throwError "failed to parse address"
