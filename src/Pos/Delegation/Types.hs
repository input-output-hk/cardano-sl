{-# LANGUAGE TemplateHaskell #-}

-- | Delegation-related network and local types.

module Pos.Delegation.Types
       (
       -- if you uncomment these, also uncomment tests
       -- in Test.Pos.Communication.Identity.BinarySpec
       --, CheckProxySKConfirmed (..)
       --, CheckProxySKConfirmedRes (..)

         DlgPayload (..)
       , mkDlgPayload
       , ProxySKLightConfirmation
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Default         (Default (def))
import           Data.List            (groupBy)
import qualified Data.Text.Buildable
import           Formatting           (bprint, int, (%))
import           Serokell.Util        (listJson)

import           Pos.Core             (ProxySKHeavy, ProxySKLight, ProxySigLight)
import           Pos.Crypto           (ProxySecretKey (..))

type ProxySKLightConfirmation = (ProxySKLight, ProxySigLight ProxySKLight)

---- | Request to check if a node has any info about PSK delivery.
--data CheckProxySKConfirmed =
--    CheckProxySKConfirmed !ProxySKLight
--    deriving (Show, Eq, Generic)
--
---- | Response to the @CheckProxySKConfirmed@ call.
--data CheckProxySKConfirmedRes =
--    CheckProxySKConfirmedRes !Bool
--    deriving (Show, Eq, Generic)

----------------------------------------------------------------------------
-- Heavyweight delegation payload
----------------------------------------------------------------------------

-- | 'DlgPayload' is put into 'MainBlock' and consists of a list of
-- heavyweight proxy signing keys. There must be no duplicates
-- (comparing by issuer) in this list.
newtype DlgPayload = UnsafeDlgPayload
    { getDlgPayload :: [ProxySKHeavy]
    } deriving (Show, Eq, NFData)

instance Default DlgPayload where
    def = UnsafeDlgPayload []

instance Buildable DlgPayload where
    build (UnsafeDlgPayload psks) =
        bprint
            ("proxy signing keys ("%int%" items): "%listJson%"\n")
            (length psks) psks

-- | Constructor of 'DlgPaylod' which ensures absence of duplicates.
mkDlgPayload :: MonadError Text m => [ProxySKHeavy] -> m DlgPayload
mkDlgPayload proxySKs = do
    unless (null duplicates) $
        throwError "Some of block's PSKs have the same issuer, which is prohibited"
    return $ UnsafeDlgPayload proxySKs
  where
    proxySKsDups psks =
        filter (\x -> length x > 1) $
        groupBy ((==) `on` pskIssuerPk) $ sortOn pskIssuerPk psks
    duplicates = proxySKsDups proxySKs

----------------------------------------------------------------------------
-- Arbitrary instances
----------------------------------------------------------------------------

--derive makeArbitrary ''CheckProxySKConfirmed
--derive makeArbitrary ''CheckProxySKConfirmedRes
