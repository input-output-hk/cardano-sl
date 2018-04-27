{-# OPTIONS_GHC -fno-warn-orphans       #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

-- Spec for testing `development` endpoints
module DevelopmentSpec where

import           Universum

import           Data.Default (def)
import           Pos.Client.KeyStorage (addSecretKey, getSecretKeysPlain)

import           Pos.Util.BackupPhrase (BackupPhrase (..), safeKeysFromPhrase)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.QuickCheck.Property (assertProperty)

import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.Pos.Configuration (withDefConfigurations)

import           Cardano.Wallet.API.Development.LegacyHandlers (deleteSecretKeys)
import           Cardano.Wallet.Server.CLI (RunMode (..))
import           Servant

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

spec :: Spec
spec =
    withCompileInfo def $
    withDefConfigurations $ \_ ->
        describe "development endpoint" $
        describe "secret-keys" $ modifyMaxSuccess (const 10) deleteAllSecretKeysSpec

deleteAllSecretKeysSpec :: (HasCompileInfo, HasConfigurations) => Spec
deleteAllSecretKeysSpec = do
    -- TODO: Use an arbitrary instance of `BackupPhrase` if available
    let phrase = BackupPhrase [ "truly", "enact", "setup", "session"
                              , "near", "film", "walk", "hard"
                              , "ginger", "audit", "regular", "any"
                              ]
    walletPropertySpec "does remove all secret keys in debug mode mode" $ do
        sKey <- either  (error "creating a secret key failed")
                        (pure . fst)
                        $ safeKeysFromPhrase mempty phrase
        lift $ addSecretKey sKey
        sKeys <- lift getSecretKeysPlain
        assertProperty (not $ null sKeys)
            "Something went wrong: Secret key has not been added."

        _ <- lift $ deleteSecretKeys DebugMode
        sKeys' <- lift getSecretKeysPlain
        assertProperty (null sKeys')
            "Oooops, secret keys not have been deleted in debug mode"

    walletPropertySpec "does not delete secret keys in production mode" $ do
        sKey <- either  (error "creating a secret key failed")
                        (pure . fst)
                        $ safeKeysFromPhrase mempty phrase
        lift $ addSecretKey sKey
        sKeys <- lift getSecretKeysPlain
        assertProperty (not $ null sKeys)
            "Something went wrong: Secret key has not been added."
        _ <- lift $ catch (deleteSecretKeys ProductionMode) (\(_ :: SomeException) -> pure NoContent)
        -- ^ Catch `ServantErr` throwing from `deleteSecretKeys` to not fail the test before end
        sKeys' <- lift getSecretKeysPlain
        assertProperty (not $ null sKeys')
            "Oooops, secret keys have been deleted in production mode"
