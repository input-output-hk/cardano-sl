{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Safe/secure logging

module Pos.Core.Util.LogSafe
       ( -- * Logging functions
         SelectiveLogWrapped(..)
       , logMessageS
       , logDebugS
       , logInfoS
       , logNoticeS
       , logWarningS
       , logErrorS
       , logMessageUnsafeP
       , logDebugUnsafeP
       , logInfoUnsafeP
       , logNoticeUnsafeP
       , logWarningUnsafeP
       , logErrorUnsafeP
       , logMessageSP
       , logDebugSP
       , logInfoSP
       , logNoticeSP
       , logWarningSP
       , logErrorSP

         -- * Secure 'Buildable's
       , SecureLog (..)
       , LogSecurityLevel
       , secure
       , unsecure

         -- ** Secure formatters
       , secureF
       , secureMaybeF
       , plainOrSecureF
       , secretOnlyF
       , secureListF
       , buildSafe
       , buildSafeMaybe
       , buildSafeList

         -- ** Secure log utils
       , BuildableSafe
       , BuildableSafeGen (..)
       , SecuredText
       , buildUnsecure
       , getSecuredText
       , deriveSafeBuildable
       , selectPublicLogs
       , selectSecretLogs
       , logMCond
       ) where

-- Universum has its own Rube Goldberg variant of 'Foldable' which we do not
-- want. It would be great if we could write
--   import           Universum hiding (toList, foldMap)
-- but HLint insists that this is not OK because toList and foldMap are never
-- used unqualified. The hiding in fact makes it clearer for the human reader
-- what's going on.
import           Universum

import           Formatting (bprint, build, fconst, later, mapf, (%))
import qualified Formatting.Buildable
import           Formatting.Internal (Format (..))

import           Pos.Core (Timestamp, TxId)
import           Pos.Core.Common (Address, Coin)
import           Pos.Crypto (PassPhrase)

import           Pos.Util.Log.LogSafe

-- | additional instances

{-
instance Buildable [Address] where
    build = bprint listJson

instance BuildableSafe a => Buildable (SecureLog [a]) where
    build = bprint (buildSafeList secure) . getSecureLog

instance Buildable (SecureLog PassPhrase) where
    build _ = "<passphrase>"

-- maybe I'm wrong here, but currently masking it important for wallet servant logs
instance Buildable (SecureLog Coin) where
    build _ = "? coin(s)"

instance Buildable (SecureLog Address) where
    build _ = "<address>"
-}
instance Buildable (SecureLog TxId) where
    build _ = "<txid>"

{-
instance Buildable (SecureLog Timestamp) where
    build _ = "<timestamp>"
-}
