-- | VSS certificates and secrets related stuff.

module Secrets
       ( updateUserSecretVSS
       , userSecretWithGenesisKey
       ) where

import           Data.List           ((!!))
import           Universum

import           Pos.Constants       (isDevelopment)
import           Pos.Crypto          (SecretKey, keyGen, vssKeyGen)
import           Pos.Genesis         (genesisDevSecretKeys)
import           Pos.Ssc.GodTossing  (genesisDevVssKeyPairs)
import           Pos.Util.UserSecret (UserSecret, usPrimKey, usVss, writeUserSecret)

import           NodeOptions         (Args (..))

userSecretWithGenesisKey
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m (SecretKey, UserSecret)
userSecretWithGenesisKey Args{..} userSecret
    | isDevelopment = case devSpendingGenesisI of
          Nothing -> fetchPrimaryKey userSecret
          Just i -> do
              let sk = genesisDevSecretKeys !! i
                  us = userSecret & usPrimKey .~ Just sk
              writeUserSecret us
              return (sk, us)
    | otherwise = fetchPrimaryKey userSecret

updateUserSecretVSS
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m UserSecret
updateUserSecretVSS Args{..} us
    | isDevelopment = case devVssGenesisI of
          Nothing -> fillUserSecretVSS us
          Just i  -> return $ us & usVss .~ Just (genesisDevVssKeyPairs !! i)
    | otherwise = fillUserSecretVSS us

fetchPrimaryKey :: (MonadIO m, MonadFail m) => UserSecret -> m (SecretKey, UserSecret)
fetchPrimaryKey userSecret = case userSecret ^. usPrimKey of
    Just sk -> return (sk, userSecret)
    Nothing -> do
        putText "Found no signing keys in keyfile, generating random one..."
        sk <- snd <$> keyGen
        let us = userSecret & usPrimKey .~ Just sk
        writeUserSecret us
        return (sk, us)

fillUserSecretVSS :: (MonadIO m, MonadFail m) => UserSecret -> m UserSecret
fillUserSecretVSS userSecret = case userSecret ^. usVss of
    Just _  -> return userSecret
    Nothing -> do
        putText "Found no VSS keypair in keyfile, generating random one..."
        vss <- vssKeyGen
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

