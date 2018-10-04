{-# OPTIONS_GHC -fno-warn-orphans       #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Spec for testing `development` endpoints
module DevelopmentSpec (spec) where

import           Universum

import           Pos.Client.KeyStorage (getSecretKeysPlain)
import           Pos.Wallet.Web.Account (genSaveRootKey)

import           Pos.Launcher (HasConfigurations)
import           Test.Pos.Util.QuickCheck.Property (assertProperty)

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.Pos.Configuration (withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)

import           Cardano.Wallet.API.Development.LegacyHandlers
                     (deleteSecretKeys)
import           Cardano.Wallet.Server.CLI (RunMode (..))
import           Data.Default (def)
import           Servant

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

spec :: Spec
spec =
    withDefConfigurations $ \_ _ ->
        describe "development endpoint" $
        describe "secret-keys" $ modifyMaxSuccess (const 10) deleteAllSecretKeysSpec

deleteAllSecretKeysSpec :: (HasConfigurations) => Spec
deleteAllSecretKeysSpec = do
    walletPropertySpec "does remove all secret keys in debug mode mode" $ do
        void $ lift $ genSaveRootKey mempty def
        sKeys <- lift getSecretKeysPlain
        assertProperty (not $ null sKeys)
            "Something went wrong: Secret key has not been added."

        _ <- lift $ deleteSecretKeys DebugMode
        sKeys' <- lift getSecretKeysPlain
        assertProperty (null sKeys')
            "Oooops, secret keys not have been deleted in debug mode"

    walletPropertySpec "does not delete secret keys in production mode" $ do
        void $ lift $ genSaveRootKey mempty def
        sKeys <- lift getSecretKeysPlain
        assertProperty (not $ null sKeys)
            "Something went wrong: Secret key has not been added."
        _ <- lift $ catch (deleteSecretKeys ProductionMode) (\(_ :: SomeException) -> pure NoContent)
        -- ^ Catch `ServantErr` throwing from `deleteSecretKeys` to not fail the test before end
        sKeys' <- lift getSecretKeysPlain
        assertProperty (not $ null sKeys')
            "Oooops, secret keys have been deleted in production mode"
