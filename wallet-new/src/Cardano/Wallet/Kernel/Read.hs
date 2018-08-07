-- | Read-only access to the DB
module Cardano.Wallet.Kernel.Read (
    -- * Read-only access to the DB
    DB -- opaque
    -- ** Helper
  , getWalletCredentials
    -- ** The only effectful getter you will ever need
  , getWalletSnapshot
    -- ** Pure getters acting on a DB snapshot
  , module Getters
  ) where

import           Universum hiding (State)

import           Data.Acid.Advanced (query')
import           Formatting (sformat, (%))
import           Serokell.Util (listJson)

import           Pos.Crypto (EncryptedSecretKey)
import           Pos.Util.Trace.Named (logError)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, Snapshot (..))
import           Cardano.Wallet.Kernel.DB.Read as Getters
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (WalletId (..))

-- | The only effectful query on this 'PassiveWallet'.
getWalletSnapshot :: PassiveWallet -> IO DB
getWalletSnapshot pw = query' (pw ^. wallets) Snapshot

-- | Get wallet credentials
--
-- For wallets without a corresponding secret key we log an error. This
-- indicates a bug somewhere, but there is not much we can do about it here,
-- since this runs in the context of applying a block.
getWalletCredentials :: PassiveWallet
                     -> IO [(WalletId, EncryptedSecretKey)]
getWalletCredentials pw = do
    snapshot         <- getWalletSnapshot pw
    (creds, missing) <- fmap partitionEithers $
      forM (walletIds snapshot) $ \walletId ->
        aux walletId <$> Keystore.lookup walletId (pw ^. walletKeystore)
    unless (null missing) $ logError (pw ^. walletLogMessage) (errMissing missing)
    return creds
  where
    aux :: WalletId
        -> Maybe EncryptedSecretKey
        -> Either (WalletId, EncryptedSecretKey) WalletId
    aux walletId Nothing    = Right walletId
    aux walletId (Just esk) = Left (walletId, esk)

    errMissing :: [WalletId] -> Text
    errMissing = sformat ("Root key missing for " % listJson)
