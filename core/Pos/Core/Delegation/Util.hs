-- | Delegation helpers.

module Pos.Core.Delegation.Util
       ( mkDlgPayload
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.List (groupBy)

import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Delegation.Types (DlgPayload (..), ProxySKHeavy)
import           Pos.Crypto (ProxySecretKey (..))

-- | Constructor of 'DlgPayload' which ensures absence of duplicates.
mkDlgPayload :: (HasConfiguration, MonadError Text m) => [ProxySKHeavy] -> m DlgPayload
mkDlgPayload proxySKs = do
    unless (null duplicates) $
        throwError "Some of block's PSKs have the same issuer, which is prohibited"
    return $ UnsafeDlgPayload proxySKs
  where
    proxySKsDups psks =
        filter (\x -> length x > 1) $
        groupBy ((==) `on` pskIssuerPk) $ sortOn pskIssuerPk psks
    duplicates = proxySKsDups proxySKs
