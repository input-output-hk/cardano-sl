module Integration.Expectations
    ( MonadIntegrationExpectations(..)
    ) where

import           Universum

import           Cardano.Wallet.Client.Http

import           Integration.Util (Resp', asksM, fromResp)

import qualified Test.Hspec as Hspec


class MonadIntegrationExpectations m where
    addressShouldExists :: Resp' WalletAddress -> m ()

instance MonadIntegrationExpectations (ReaderT (WalletClient IO) IO) where
    addressShouldExists resp = do
        addr <- fromResp resp
        addrs <- asksM getAddressIndex >>= fromResp
        [ addrId | WalletAddress{..} <- addrs] `shouldContain` [addrId addr]


--
-- INTERNALS
--

shouldContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> ReaderT r IO ()
shouldContain xs =
    lift . Hspec.shouldContain xs
