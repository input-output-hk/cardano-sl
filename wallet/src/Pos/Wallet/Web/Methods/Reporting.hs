{-# LANGUAGE TypeFamilies #-}

-- | Crashes reporting

module Pos.Wallet.Web.Methods.Reporting
       ( reportingInitialized
       ) where

import           Universum

import           Pos.Reporting.Methods      (reportInfo)
import           Pos.Wallet.Web.ClientTypes (CInitialized)
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)

reportingInitialized :: MonadWalletWebMode m => CInitialized -> m ()
reportingInitialized cinit = reportInfo False (show cinit)
