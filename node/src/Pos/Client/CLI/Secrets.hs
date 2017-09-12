-- | VSS certificates and secrets related stuff.

module Pos.Client.CLI.Secrets
       ( updateUserSecretVSS
       , userSecretWithGenesisKey
       ) where

import           Data.List                  ((!!))
import           Universum

import           Pos.Constants              (isDevelopment)
import           Pos.Crypto                 (SecretKey, keyGen, runSecureRandom,
                                             vssKeyGen)
import           Pos.Genesis                (genesisDevSecretKeys)
import           Pos.Ssc.GodTossing         (genesisDevVssKeyPairs)
import           Pos.Util.UserSecret        (UserSecret, usPrimKey, usVss,
                                             writeUserSecret)

import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..))

userSecretWithGenesisKey
    :: (MonadIO m) => CommonNodeArgs -> UserSecret -> m (SecretKey, UserSecret)
userSecretWithGenesisKey CommonNodeArgs{..} userSecret
    | isDevelopment = case devSpendingGenesisI of
          Nothing -> fetchPrimaryKey userSecret
          Just i -> do
              let sk = genesisDevSecretKeys !! i
                  us = userSecret & usPrimKey .~ Just sk
              writeUserSecret us
              return (sk, us)
    | otherwise = fetchPrimaryKey userSecret

updateUserSecretVSS
    :: (MonadIO m) => CommonNodeArgs -> UserSecret -> m UserSecret
updateUserSecretVSS CommonNodeArgs{..} us
    | isDevelopment = case devVssGenesisI of
          Nothing -> fillUserSecretVSS us
          Just i  -> return $ us & usVss .~ Just (genesisDevVssKeyPairs !! i)
    | otherwise = fillUserSecretVSS us

fetchPrimaryKey :: (MonadIO m) => UserSecret -> m (SecretKey, UserSecret)
fetchPrimaryKey userSecret = case userSecret ^. usPrimKey of
    Just sk -> return (sk, userSecret)
    Nothing -> do
        putText "Found no signing keys in keyfile, generating random one..."
        sk <- snd <$> liftIO (runSecureRandom keyGen)
        let us = userSecret & usPrimKey .~ Just sk
        writeUserSecret us
        return (sk, us)

fillUserSecretVSS :: (MonadIO m) => UserSecret -> m UserSecret
fillUserSecretVSS userSecret = case userSecret ^. usVss of
    Just _  -> return userSecret
    Nothing -> do
        putText "Found no VSS keypair in keyfile, generating random one..."
        vss <- liftIO (runSecureRandom vssKeyGen)
        let us = userSecret & usVss .~ Just vss
        writeUserSecret us
        return us

-- processUserSecret
--     :: (MonadIO m, MonadFail m)
--     => Args -> UserSecret -> m (SecretKey, UserSecret)
-- processUserSecret args@Args {..} userSecret = case backupPhrase of
--     Nothing -> updateUserSecretVSS args userSecret >>= userSecretWithGenesisKey args
--     Just ph -> do
--         (sk, vss) <- either keyFromPhraseFailed pure $ keysFromPhrase ph
--         let us = userSecret & usPrimKey .~ Just sk & usVss .~ Just vss
--         writeUserSecret us
--         return (sk, us)
--   where
--     keyFromPhraseFailed msg = fail $ "Key creation from phrase failed: " <> show msg
