{-- | An opaque handle to a keystore, used to read and write 'EncryptedSecretKey'
      from/to disk.
--}

{-# LANGUAGE BangPatterns #-}

module Cardano.Wallet.Kernel.Keystore (
      Keystore -- opaque
    , newKeystore
    , legacyKeystore
    -- * Inserting values
    , insert
    -- * Queries on a keystore
    , lookupKey
    -- * Conversions
    , toList
    ) where

import           Universum hiding (toList)

import           Control.Concurrent (modifyMVar_, withMVar)
import           Control.Lens (mapped)
import qualified Data.List

import           Pos.Context (NodeContext (ncUserSecret))
import           Pos.Crypto (EncryptedSecretKey, hash)
import           Pos.Util.UserSecret (UserSecret, peekUserSecret, usKeys)
import           System.Wlog (WithLogger)

import           Cardano.Wallet.Kernel.DB.HdWallet (eskToHdRootId)
import           Cardano.Wallet.Kernel.Types (WalletId (..))

-- Internal storage necessary to smooth out the legacy 'UserSecret' API.
data InternalStorage = InternalStorage {
      _storageSecret      :: !UserSecret
    }

data Keystore = Keystore (MVar InternalStorage)

-- | Creates a new keystore.
newKeystore :: (MonadIO m, WithLogger m) => FilePath -> m Keystore
newKeystore fp = do
    us <- peekUserSecret fp
    Keystore <$> newMVar (InternalStorage us)

-- | Creates a legacy 'Keystore' by reading the 'UserSecret' from a 'NodeContext'.
-- Hopefully this function will go in the near future.
legacyKeystore :: MonadIO m => NodeContext -> m Keystore
legacyKeystore ctx = do
     us <- atomically $ readTVar $ ncUserSecret ctx
     Keystore <$> newMVar (InternalStorage us)

-- | Insert a new 'EncryptedSecretKey' indexed by the input 'WalletId'.
insert :: MonadIO m
       => WalletId
       -> EncryptedSecretKey
       -> Keystore
       -> m ()
insert _walletId esk (Keystore ks) =
    liftIO $ modifyMVar_ ks $ \(InternalStorage us) -> do
        return $ if view usKeys us `contains` esk
                     then InternalStorage us
                     else InternalStorage (us & over usKeys (esk :))
    where
      -- Comparator taken from the old code which needs to hash
      -- all the 'EncryptedSecretKey' in order to compare them.
      contains :: [EncryptedSecretKey] -> EncryptedSecretKey -> Bool
      contains ls k = hash k `elem` map hash ls

-- | Lookup an 'EncryptedSecretKey' associated to the input 'HdRootId'.
lookupKey :: MonadIO m
          => WalletId
          -> Keystore
          -> m (Maybe EncryptedSecretKey)
lookupKey (WalletIdHdRnd walletId) (Keystore ks) =
    liftIO $ withMVar ks $ \(InternalStorage us) ->
        return $ Data.List.find (\k -> eskToHdRootId k == walletId) (us ^. usKeys)

-- | Returns all the 'EncryptedSecretKey' known to this 'Keystore'.
toList :: MonadIO m => Keystore -> m [(WalletId, EncryptedSecretKey)]
toList (Keystore ks) =
    liftIO $ withMVar ks $ \(InternalStorage us) ->
        return (over mapped (\k -> (WalletIdHdRnd (eskToHdRootId k), k)) (us ^. usKeys))
