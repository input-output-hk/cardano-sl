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
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (ProtocolMagic, encToSecret)
import           Pos.Generator.Block (BlockGenParams (..), genBlocks, tgpTxCountRange)
import           Pos.Infra.StateLock (Priority (..), withStateLock)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason (..))
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (withCompileInfo)

import           Lang.Value (GenBlocksParams (..))
import           Mode (MonadAuxxMode)


generateBlocks :: MonadAuxxMode m => ProtocolMagic -> GenBlocksParams -> m ()
generateBlocks pm GenBlocksParams{..} = withStateLock HighPriority ApplyBlock $ \_ -> do
    seed <- liftIO $ maybe randomIO pure bgoSeed
    logInfo $ "Generating with seed " <> show seed

    let nm = makeNetworkMagic pm
    allSecrets <- mkAllSecretsSimple nm . map encToSecret <$> getSecretKeysPlain

    let bgenParams =
            BlockGenParams
                { _bgpSecrets         = allSecrets
                , _bgpGenStakeholders = gdBootStakeholders genesisData
                , _bgpBlockCount      = fromIntegral bgoBlockN
                -- tx generation is disalbed for now
                , _bgpTxGenParams     = def & tgpTxCountRange .~ (0,0)
                , _bgpInplaceDB       = True
                , _bgpSkipNoKey       = True
                , _bgpTxpGlobalSettings = txpGlobalSettings pm
                }
    withCompileInfo def $ evalRandT (genBlocks pm bgenParams (const ())) (mkStdGen seed)
    -- We print it twice because there can be a ton of logs and
    -- you don't notice the first message.
    logInfo $ "Generated with seed " <> show seed
