-- | Block generation.

module Command.BlockGen
       ( generateBlocks
       ) where

import           Universum

import           Control.Monad.Random.Strict (evalRandT)
import           Data.Default (def)
import           System.Random (mkStdGen, randomIO)

import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Client.KeyStorage (getSecretKeysPlain)
import           Pos.Core (genesisData)
import           Pos.Core.Genesis (gdBootStakeholders)
import           Pos.Crypto (ProtocolMagic, encToSecret)
import           Pos.DB.GState.Lock (Priority (..), withStateLock)
import           Pos.DB.Txp (txpGlobalSettings)
import           Pos.Generator.Block (BlockGenParams (..), genBlocks,
                     tgpTxCountRange)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason (..))
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.Trace (noTrace)
import           Pos.Util.Trace.Named (TraceNamed, logInfo)

import           Lang.Value (GenBlocksParams (..))
import           Mode (MonadAuxxMode)


generateBlocks :: MonadAuxxMode m
               => TraceNamed IO
               -> ProtocolMagic
               -> TxpConfiguration
               -> GenBlocksParams -> m ()                                -- JSON logging Trace
generateBlocks logTrace pm txpConfig GenBlocksParams{..} = withStateLock noTrace HighPriority ApplyBlock $ \_ -> do
    seed <- liftIO $ maybe randomIO pure bgoSeed
    liftIO $ logInfo logTrace $ "Generating with seed " <> show seed

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
                , _bgpTxpGlobalSettings = txpGlobalSettings pm txpConfig
                }
    withCompileInfo $ evalRandT (genBlocks logTrace pm txpConfig bgenParams (const ())) (mkStdGen seed)
    -- We print it twice because there can be a ton of logs and
    -- you don't notice the first message.
    liftIO $ logInfo logTrace $ "Generated with seed " <> show seed
