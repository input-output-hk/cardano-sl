{-# OPTIONS_GHC -fno-warn-orphans       #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}


-- Spec for testing `development` endpoints
-- Heavily inspired by `TestCase_CT4.hs` (see https://iohk.myjetbrains.com/youtrack/issue/DT-4)
module DevelopmentSpec where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Control.Lens
import           Pos.Client.KeyStorage (MonadKeys (..), MonadKeysRead (..), addSecretKey,
                                        deleteSecretKeyBy, getSecretDefault, getSecretKeysPlain,
                                        modifySecretDefault)
import           Pos.Util.BackupPhrase (BackupPhrase (..), safeKeysFromPhrase)
import           Pos.Util.UserSecret
import           Pos.Wallet.Web.ClientTypes (encToCId)
import           System.Wlog (HasLoggerName (..), LoggerName (..))
import           Test.Hspec
import           Pos.Wallet.Web.State (MonadWalletDB)

import           Cardano.Wallet.API.Development.LegacyHandlers (deleteSecretKeys)
import           Cardano.Wallet.Server.CLI (RunMode (..))


testBackupPhrase :: BackupPhrase
testBackupPhrase = BackupPhrase
    [ "fun"
     , "aunt"
     , "congress"
     , "clock"
     , "page"
     , "property"
     , "sausage"
     , "net"
     , "shuffle"
     , "custom"
     , "timber"
     , "know"
     ]

newtype KeysContext = KeysContext
    { _ksUserSecrets :: TVar UserSecret
    }

makeLenses ''KeysContext

type TestMonad = ReaderT KeysContext IO

instance HasUserSecret KeysContext where
    userSecret = ksUserSecrets

instance HasLoggerName IO where
    askLoggerName = pure (LoggerName "DT-4")
    modifyLoggerName _ x =  x

runTestMonad :: TestMonad a -> IO a
runTestMonad tm = do
    us <- peekUserSecret "myTestSecrets.key"
    secrets <- newTVarIO us
    let ctx = KeysContext secrets
    runReaderT tm ctx

instance MonadKeysRead TestMonad where
    getSecret = getSecretDefault

instance MonadKeys TestMonad where
    modifySecret = modifySecretDefault

-- | Eq instance is friendly borrowed from `Test.Pos.Cbor.CborSpec`.
-- TODO: Move it to another place to share it for all specs
instance Eq CC.XPrv where
    (==) = (==) `on` CC.unXPrv

-- instance MonadWalletDB KeysContext TestMonad

spec :: Spec
spec =
    describe "development endpoint" $
        describe "secret-keys" $
            it "deletes all secret keys in debug mode" $ do
                sKeys <- runTestMonad $ do
                    skey <- either (error "test failed") (pure . fst) $ safeKeysFromPhrase mempty testBackupPhrase
                    getSecretKeysPlain
                -- a secret key should be available
                length sKeys `shouldBe` 1

                sKeys' <- runTestMonad $ do
                    _ <- deleteSecretKeys DebugMode
                    getSecretKeysPlain
                -- all secret keys should be removed
                sKeys' `shouldBe` mempty
