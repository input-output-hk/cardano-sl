{-# LANGUAGE TemplateHaskell #-}

-- | Module which provides `MonadWalletWebMode` instance for tests

module Test.Pos.Wallet.Web.Mode
       ( WalletTestParams (..)
       , HasWalletTestParams (..)
       , WalletTestMode
       , runWalletTestMode
       ) where

import           Universum

import           Control.Lens              (makeClassy, makeLensesWith)
import qualified Data.Text.Buildable
import           Formatting                (bprint, build, formatToString, (%))
import           Test.QuickCheck           (Arbitrary (..))

import           Pos.AllSecrets            (AllSecrets (..), HasAllSecrets (..))
import           Pos.Util.Util             (postfixLFields)

import           Pos.Wallet.Web.State      (WalletState, openMemState)

import           Test.Pos.Block.Logic.Mode (BlockTestContext (..), BlockTestMode,
                                            HasTestParams (..), TestParams (..),
                                            runBlockTestMode)

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

-- | This datatype contains all parameters which should be generated before
-- testing starts
data WalletTestParams = WalletTestParams
    { _wtpBlockTestParams :: !TestParams
    -- ^ Block test params
    -- TODO: add wallet-specific parameters
    }

makeClassy ''WalletTestParams

instance HasAllSecrets WalletTestParams where
    allSecrets = wtpBlockTestParams . allSecrets

instance Buildable WalletTestParams where
    build WalletTestParams {..} =
        bprint ("WalletTestParams {\n"%
                "  blockTestParams = "%build%"\n"%
                "}\n")
        _wtpBlockTestParams

instance Show WalletTestParams where
    show = formatToString build

instance Arbitrary WalletTestParams where
    arbitrary = WalletTestParams <$> arbitrary

----------------------------------------------------------------------------
-- Wallet context
----------------------------------------------------------------------------

data WalletTestContext = WalletTestContext
    { wtcWalletState :: !WalletState
    }

makeLensesWith postfixLFields ''WalletTestContext

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initWalletTestContext
    :: HasCoreConstants
    => WalletTestParams
    -> (WalletTestContext -> BlockTestMode a)
    -> BlockTestMode a
initWalletTestContext wtp@WalletTestParams {..} callback = do
    wState <- openMemState
    let wCtx = WalletTestContext
            { wtcWalletState = wState
            }
    callback wCtx

type WalletTestMode = ReaderT WalletTestContext BlockTestMode

runWalletTestMode
    :: HasCoreConstants
    => WalletTestParams
    -> WalletTestMode a
    -> IO a
runWalletTestMode wtp action =
    runBlockTestMode (wtp ^. wtpBlockTestParams) $
    initWalletTestContext wtp (runReaderT action)
