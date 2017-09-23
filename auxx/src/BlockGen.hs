-- | Block generation.

module BlockGen
       ( generateBlocks
       ) where

import           Universum

import           Control.Monad.Random.Strict (evalRandT)
import           Data.Default                (def)
import           System.Random               (mkStdGen, randomIO)

import           Pos.AllSecrets              (mkAllSecretsSimple)
import           Pos.Client.KeyStorage       (getAllUserSecrets, getSecretKeys)
import           Pos.Core                    (gdBootStakeholders, genesisData)
import           Pos.Crypto                  (encToSecret)
import           Pos.Generator.Block         (BlockGenParams (..), genBlocks)
import           Pos.Launcher                (HasConfigurations)
import           Pos.Util.CompileInfo        (withCompileInfo)

import           Command.Types               (GenBlocksParams (..))
import           Mode                        (AuxxMode)


generateBlocks :: HasConfigurations => GenBlocksParams -> AuxxMode ()
generateBlocks GenBlocksParams{..} = do
    seed <- liftIO $ maybe randomIO pure bgoSeed
    putText $ "Generating with seed " <> show seed

    allSecrets <- mkAllSecretsSimple . map encToSecret . getAllUserSecrets <$> getSecretKeys

    let bgenParams =
            BlockGenParams
                { _bgpSecrets         = allSecrets
                , _bgpGenStakeholders = gdBootStakeholders genesisData
                , _bgpBlockCount      = fromIntegral bgoBlockN
                , _bgpTxGenParams     = def
                , _bgpInplaceDB       = True
                , _bgpSkipNoKey       = True
                }
    withCompileInfo def $ void $ evalRandT (genBlocks bgenParams) (mkStdGen seed)
    -- We print it twice because there can be a ton of logs and
    -- you don't notice the first message.
    putText $ "Generated with seed " <> show seed
