{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wallet
  ( prop_test_fail
  , prop_test_ok
  , idempotentIOProperty
  )
  where

import           System.IO (hPutStrLn)
import           Universum

import           Data.TreeDiff (ToExpr (toExpr))
import           GHC.Generics (Generic, Generic1)
import           Test.QuickCheck (Gen, arbitrary, frequency, generate,
                     ioProperty, oneof, (===))
import           Test.QuickCheck.Gen.Unsafe (promote)
import           Test.QuickCheck.Monadic (monadicIO)
import           Test.QuickCheck.Property

import           Test.StateMachine
import           Test.StateMachine.Types (Command (..), Commands (..),
                     StateMachine)

import qualified Test.StateMachine.Types.Rank2 as Rank2

import           Control.Concurrent (threadDelay)

------------------------------------------------------------------------

-- Wallet actions

data Action (r :: * -> *)
    = ResetWalletA
    deriving (Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

data Response (r :: * -> *)
    = ResetWalletR
    deriving (Show, Generic1, Rank2.Foldable)


------------------------------------------------------------------------

-- Wallet state

data Model (r :: * -> *) = Model
    deriving (Eq, Show, Generic)

deriving instance ToExpr (Model Concrete)

initModel :: Model r
initModel = Model

-- If you need more fine grained distribution, use preconditions
preconditions :: Model Symbolic -> Action Symbolic -> Logic
preconditions _ _      = Top

transitions :: Model r -> Action r -> Response r -> Model r
transitions _ _ _ = Model

postconditions :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
postconditions _ _ _ = Bot

------------------------------------------------------------------------

-- Action generator


generator :: Model Symbolic -> Gen (Action Symbolic)
-- if wallet has not been reset, then we should first reset it!
generator _ = pure ResetWalletA

shrinker :: Action Symbolic -> [Action Symbolic]
shrinker _ = []

-- ------------------------------------------------------------------------
--
semantics :: Handle -> Action Concrete -> IO (Response Concrete)
semantics h cmd = case cmd of
    ResetWalletA -> do
        -- Error seems to be goone if line bellow is removed?
        hPutStrLn h "oooo"
        return ResetWalletR

mock :: Model Symbolic -> Action Symbolic -> GenSym (Response Symbolic)
mock _ _      = pure ResetWalletR

------------------------------------------------------------------------

-- TODO: model invariant?
-- TODO: model distribution?
stateMachine :: Handle -> StateMachine Model Action IO Response
stateMachine h =
    StateMachine
        initModel
        transitions
        preconditions
        postconditions
        Nothing
        generator
        Nothing
        shrinker
        (semantics h)
        mock

sleepSeconds :: MonadIO m => Integer -> m ()
sleepSeconds sec =
    liftIO . delay $ sec * 1000 * 1000
  where
    delay time = do
        let maxWait = min time $ toInteger (maxBound :: Int)
        liftIO $ threadDelay (fromInteger maxWait)
        when (maxWait /= time) $ delay (time - maxWait)

prop_test_ok :: Handle -> Property
prop_test_ok h = monadicIO $ do
    -- artificial delay
    -- it works even with the delay
    liftIO $ sleepSeconds 1
    let cmds = Commands [Command ResetWalletA mempty]
    print $ commandNamesInOrder cmds
    (hist, _, res) <- runCommands sm cmds
    prettyCommands sm hist $
        checkCommandNames cmds (res === Ok)
  where
    sm = stateMachine h

prop_test_fail :: Handle -> Property
prop_test_fail h = forAllCommands sm (Just 10) $ \cmds -> monadicIO $ do
    print $ commandNamesInOrder cmds
    (hist, _, res) <- runCommands sm cmds
    prettyCommands sm hist $
        checkCommandNames cmds (res === Ok)
  where
    sm = stateMachine h

-- NOTE: copied from quickcheck-2.12.6.1
idempotentIOProperty :: Testable prop => IO prop -> Property
idempotentIOProperty =
  MkProperty . fmap (MkProp . ioRose . fmap unProp) .
  promote . fmap (unProperty . property)
