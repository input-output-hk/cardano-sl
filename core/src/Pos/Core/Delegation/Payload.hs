module Pos.Core.Delegation.Payload
       ( DlgPayload (..)
       , checkDlgPayload
       ) where

import           Universum

import           Control.Monad.Except (MonadError, throwError)
import           Data.Default (Default (def))
import qualified Data.Text.Buildable
import           Formatting (bprint, int, (%))
import           Serokell.Util (allDistinct, listJson)

import           Pos.Binary.Class (Bi (..))
import           Pos.Crypto (ProtocolMagic, ProxySecretKey (..), validateProxySecretKey)

import           Pos.Core.Delegation.HeavyDlgIndex

-- | 'DlgPayload' is put into 'MainBlock' and is a set of heavyweight
-- proxy signing keys. List of psk issuers should be unique also.
newtype DlgPayload = UnsafeDlgPayload
    { getDlgPayload :: [ProxySKHeavy]
    } deriving (Show, Eq, Generic, NFData)

instance Default DlgPayload where
    def = UnsafeDlgPayload mempty

instance Buildable DlgPayload where
    build (UnsafeDlgPayload psks) =
        bprint
            ("proxy signing keys ("%int%" items): "%listJson%"\n")
            (length psks) psks

instance Bi DlgPayload where
    encode = encode . getDlgPayload
    decode = UnsafeDlgPayload <$> decode

checkDlgPayload
    :: MonadError Text m
    => ProtocolMagic
    -> DlgPayload
    -> m ()
checkDlgPayload protocolMagic (UnsafeDlgPayload proxySKs) = do
    unless (allDistinct $ map pskIssuerPk proxySKs) $
        throwError "Some of block's PSKs have the same issuer, which is prohibited"
    forM_ proxySKs (validateProxySecretKey protocolMagic)
