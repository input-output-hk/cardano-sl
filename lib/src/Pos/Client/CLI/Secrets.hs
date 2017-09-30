-- | VSS certificates and secrets related stuff.

module Pos.Client.CLI.Secrets
       ( updateUserSecretVSS
       , userSecretWithGenesisKey
       ) where

import           Data.List                  ((!!))
import           Universum

import           Pos.Core                   (HasConfiguration, generatedSecrets,
                                             genesisSecrets)
import           Pos.Crypto                 (SecretKey, keyGen, runSecureRandom,
                                             vssKeyGen)
import           Pos.Util.UserSecret        (UserSecret, usPrimKey, usVss,
                                             writeUserSecret)

import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..))

userSecretWithGenesisKey
    :: (HasConfiguration, MonadIO m) => CommonNodeArgs -> UserSecret -> m (SecretKey, UserSecret)
userSecretWithGenesisKey CommonNodeArgs{..} userSecret
    | Just i <- devGenesisSecretI,
      Just secretKeys <- genesisSecrets = do
        let sk = view _1 (secretKeys !! i)
            us = userSecret & usPrimKey .~ Just sk
        writeUserSecret us
        pure (sk, us)
    | Just _ <- devGenesisSecretI, Nothing <- generatedSecrets =
        error "devGenesisSecretI is specified, but no generatedSecrets is present.\n\
              \Try to change initializer in genesis spec"
    | otherwise = fetchPrimaryKey userSecret

updateUserSecretVSS
    :: (HasConfiguration, MonadIO m) => CommonNodeArgs -> UserSecret -> m UserSecret
updateUserSecretVSS CommonNodeArgs{..} us
    | Just i <- devGenesisSecretI,
      Just secretKeys <- genesisSecrets =
        pure $ us & usVss .~ Just (view _3 $ secretKeys !! i)
    | Just _ <- devGenesisSecretI, Nothing <- generatedSecrets =
        error "devGenesisSecretI is specified, but secret keys are unknown.\n\
              \Try to change initializer in genesis spec"
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
