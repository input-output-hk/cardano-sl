{-# LANGUAGE TypeFamilies #-}

-- | Crashes reporting

module Pos.Wallet.Web.Methods.Reporting
       ( reportingInitialized
       ) where

import           Universum

import           Pos.Reporting.Methods (MonadReporting, reportInfo)
import           Pos.Wallet.Web.ClientTypes (CInitialized)
import           Servant.API.ContentTypes (NoContent (..))

-- REPORT:INFO Time to initialize Daedalus info (from start to main screen, from start to network connection established)
reportingInitialized :: (Monad m, MonadReporting m) => CInitialized -> m NoContent
reportingInitialized cinit = do
    reportInfo (show cinit)
    return NoContent
