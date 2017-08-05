{-# LANGUAGE TypeFamilies #-}

-- | Crashes reporting

module Pos.Wallet.Web.Methods.Reporting
       ( reportingInitialized
       ) where

import           Universum

import           Formatting                 (sformat, shown, (%))
import           Pos.ReportServer.Report    (ReportType (RInfo))
import           System.Wlog                (logError)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Reporting.Methods      (sendReportNodeNologs)
import           Pos.Wallet.Web.ClientTypes (CInitialized)
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)


reportingInitialized :: MonadWalletWebMode m => CInitialized -> m ()
reportingInitialized cinit = do
    sendReportNodeNologs (RInfo $ show cinit) `catchAll` handler
  where
    handler e =
        logError $
        sformat ("Didn't manage to report initialization time "%shown%
                 " because of exception "%shown) cinit e
