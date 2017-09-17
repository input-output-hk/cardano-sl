-- | Former avvm-migration script. Parses rscoin-dump to create
-- genesis block of CSL.

module Avvm
       ( applyBlacklisted
       ) where

import           Universum

import           Data.List         ((\\))
import           System.Wlog       (WithLogger, logInfo)

import           Pos.Aeson.Genesis (fromAvvmPk)
import           Pos.Core.Genesis  (AvvmData (..), aePublicKey)


-- | Applies blacklist to avvm utxo, produces warnings and stats about
-- how much was deleted.
applyBlacklisted ::
       (WithLogger m, MonadIO m, MonadThrow m, MonadFail m)
    => Maybe FilePath
    -> AvvmData
    -> m AvvmData
applyBlacklisted Nothing r = r <$ logInfo "Blacklisting: file not specified, skipping"
applyBlacklisted (Just blacklistPath) AvvmData{..} = do
    addrTexts <- lines <$> readFile blacklistPath
    blacklisted <- mapM fromAvvmPk addrTexts
    let filteredBad = filter ((`elem` blacklisted) . aePublicKey) avvmData
    let filtered = avvmData \\ filteredBad
    logInfo $
        "Removing " <> show (length filteredBad) <> " entries from utxo (out of " <>
        show (length blacklisted) <> " total entries in the blacklist)"
    pure $ AvvmData filtered
