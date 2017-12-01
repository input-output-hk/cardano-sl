-- | Block generation.

module Command.BlockGen
       ( generateBlocks
       ) where

import           Universum

import           Control.Monad.Random.Strict (evalRandT)
import           Data.Default (def)
import           System.Random (mkStdGen, randomIO)
import           System.Wlog (logInfo)

import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Client.KeyStorage (getSecretKeysPlain)
import           Pos.Core (gdBootStakeholders, genesisData)
import           Pos.Crypto (encToSecret)
import           Pos.Generator.Block (BlockGenParams (..), genBlocks, tgpTxCountRange)
import           Pos.StateLock (Priority (..), withStateLock)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (withCompileInfo)

import           Lang.Value (GenBlocksParams (..))
import           Mode (MonadAuxxMode)


generateBlocks :: MonadAuxxMode m => GenBlocksParams -> m ()
generateBlocks GenBlocksParams{..} = withStateLock HighPriority "auxx" $ \_ -> do
    seed <- liftIO $ maybe randomIO pure bgoSeed
    logInfo $ "Generating with seed " <> show seed

    allSecrets <- mkAllSecretsSimple . map encToSecret <$> getSecretKeysPlain

    let bgenParams =
            BlockGenParams
                { _bgpSecrets         = allSecrets
                , _bgpGenStakeholders = gdBootStakeholders genesisData
                , _bgpBlockCount      = fromIntegral bgoBlockN
                -- tx generation is disalbed for now
                , _bgpTxGenParams     = def & tgpTxCountRange .~ (0,0)
                , _bgpInplaceDB       = True
                , _bgpSkipNoKey       = True
                , _bgpTxpGlobalSettings = txpGlobalSettings
                }
    withCompileInfo def $ evalRandT (genBlocks bgenParams (const ())) (mkStdGen seed)
    -- We print it twice because there can be a ton of logs and
    -- you don't notice the first message.
    logInfo $ "Generated with seed " <> show seed
