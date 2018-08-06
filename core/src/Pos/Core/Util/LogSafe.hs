{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Safe/secure logging

module Pos.Core.Util.LogSafe
       ( -- * Logging functions
         logMessageS
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
       , deriveSafeBuildable
       ) where

-- Universum has its own Rube Goldberg variant of 'Foldable' which we do not
-- want. It would be great if we could write
--   import           Universum hiding (toList, foldMap)
-- but HLint insists that this is not OK because toList and foldMap are never
-- used unqualified. The hiding in fact makes it clearer for the human reader
-- what's going on.
import           Universum

import qualified Formatting.Buildable

import           Pos.Core.Common (Address, Coin)
import           Pos.Core.Slotting (Timestamp)
import           Pos.Core.Txp (TxId)
import           Pos.Crypto (PassPhrase)

-- this is where LogSafe internals are implemented
import           Pos.Util.Log.LogSafe

-- | additional instances for

instance Buildable (SecureLog Text) where
    build _ = "<hidden>"

instance Buildable (SecureLog PassPhrase) where
    build _ = "<passphrase>"

-- maybe I'm wrong here, but currently masking it important for wallet servant logs
instance Buildable (SecureLog Coin) where
    build _ = "? coin(s)"

instance Buildable (SecureLog Address) where
    build _ = "<address>"

instance Buildable (SecureLog Word32) where
    build _ = "<bytes>"

instance Buildable (SecureLog TxId) where
    build _ = "<txid>"

instance Buildable (SecureLog Timestamp) where
    build _ = "<timestamp>"
