-- | VSS certificates and secrets related stuff.

module Secrets
       ( updateUserSecretVSS
       , userSecretWithGenesisKey
       ) where

import           Universum

import           Pos.Crypto          (SecretKey, keyGen, vssKeyGen)
import           Pos.Util.UserSecret (UserSecret, usPrimKey, usVss, writeUserSecret)

import           ExplorerOptions     (Args (..))

userSecretWithGenesisKey
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m (SecretKey, UserSecret)
userSecretWithGenesisKey _ = fetchPrimaryKey

updateUserSecretVSS
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m UserSecret
updateUserSecretVSS _ = fillUserSecretVSS

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

