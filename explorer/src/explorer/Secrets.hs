-- | VSS certificates and secrets related stuff.

module Secrets
       ( updateUserSecretVSS
       , userSecretWithGenesisKey
       ) where

import           Universum

import           Pos.Crypto          (SecretKey, keyGen, runSecureRandom, vssKeyGen)
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
        sk <- snd <$> liftIO (runSecureRandom keyGen)
        let us = userSecret & usPrimKey .~ Just sk
        writeUserSecret us
        return (sk, us)

fillUserSecretVSS :: (MonadIO m, MonadFail m) => UserSecret -> m UserSecret
fillUserSecretVSS userSecret = case userSecret ^. usVss of
    Just _  -> return userSecret
    Nothing -> do
        putText "Found no VSS keypair in keyfile, generating random one..."
        vss <- liftIO (runSecureRandom vssKeyGen)
        let us = userSecret & usVss .~ Just vss
        writeUserSecret us
        return us
