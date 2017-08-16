{-# LANGUAGE TypeFamilies #-}

-- | Crashes reporting

module Pos.Wallet.Web.Methods.Reporting
       ( reportingInitialized
       , reportingElectroncrash
       ) where

import           Universum

import           Control.Monad.Catch        (SomeException, try)
import qualified Data.List.NonEmpty         as NE
import           Formatting                 (sformat, shown, (%))
import           Pos.ReportServer.Report    (ReportType (RInfo))
import           Servant.Multipart          (fdFilePath)
import           System.Wlog                (logError)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Aeson.WalletBackup     ()
import           Pos.Reporting.MemState     (HasReportServers (..),
                                             HasReportingContext (..))
import           Pos.Reporting.Methods      (sendReport, sendReportNodeNologs)
import           Pos.Wallet.Web.ClientTypes (CElectronCrashReport, CInitialized,
                                             cecUploadDump)
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)


reportingInitialized :: MonadWalletWebMode m => CInitialized -> m ()
reportingInitialized cinit = do
    sendReportNodeNologs (RInfo $ show cinit) `catchAll` handler
  where
    handler e =
        logError $
        sformat ("Didn't manage to report initialization time "%shown%
                 " because of exception "%shown) cinit e

reportingElectroncrash :: MonadWalletWebMode m => CElectronCrashReport -> m ()
reportingElectroncrash celcrash = do
    servers <- view (reportingContext . reportServers)
    errors <- fmap lefts $ forM servers $ \serv ->
        try $ sendReport [fdFilePath $ cecUploadDump celcrash]
                         []
                         (RInfo $ show celcrash)
                         "daedalus"
                         (toString serv)
    whenNotNull errors $ handler . NE.head
  where
    fmt = ("Didn't manage to report electron crash "%shown%" because of exception "%shown)
    handler :: MonadWalletWebMode m => SomeException -> m ()
    handler e = logError $ sformat fmt celcrash e

