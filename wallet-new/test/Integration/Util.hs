module Integration.Util where

import           Universum

import           Cardano.Wallet.Client.Http (ClientError, WalletResponse (..))


type Resp' a = Either ClientError (WalletResponse a)

runSpec :: r -> ReaderT r IO () -> IO ()
runSpec =
    flip runReaderT

fromResp :: MonadThrow m => Resp' a -> ReaderT r m a
fromResp =
    lift . either throwM (return . wrData)

asksM :: Monad m => (r -> m a) -> ReaderT r m a
asksM fn =
    ask >>= lift . fn
