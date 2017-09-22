module Main where

import           Universum

import           Control.Monad.Random.Strict (evalRandT)
import qualified Data.Map                    as M
import           Formatting                  (build, sformat, (%))
import           Mockable                    (runProduction)
import           System.Directory            (doesDirectoryExist)
import           System.Random               (mkStdGen, randomIO)
import           System.Wlog                 (usingLoggerName)

import           Pos.AllSecrets              (mkAllSecretsSimple)
import           Pos.Core                    (gdBootStakeholders, genesisData)
import           Pos.DB                      (closeNodeDBs, openNodeDBs)
import           Pos.Generator.Block         (BlockGenParams (..), genBlocks)
import           Pos.Launcher                (withConfigurations)
import           Pos.Txp.GenesisUtxo         (genesisUtxo)
import           Pos.Txp.Toil                (GenesisUtxo (..))
import           Pos.Util.UserSecret         (peekUserSecret, usPrimKey)

import           Context                     (initTBlockGenMode)
import           Error                       (TBlockGenError (..))
import           Options                     (BlockGenOptions (..), getBlockGenOptions)

main :: IO ()
main = getBlockGenOptions >>= \(BlockGenOptions {..}) ->
  flip catch catchEx $ usingLoggerName "block-gen" $ withConfigurations bgoConfigurationOptions $ do
    seed <- liftIO $ maybe randomIO pure bgoSeed
    putText $ "Generating with seed " <> show seed

    liftIO $ when bgoAppend $ checkExistence bgoPath

    allSecrets <- mkAllSecretsSimple <$> do
            when (null bgoSecrets) $ throwM NoOneSecrets
            mapM parseSecret bgoSecrets

    when (M.null $ unGenesisUtxo genesisUtxo) $ throwM EmptyUtxo

    let bgenParams =
            BlockGenParams
                { _bgpSecrets         = allSecrets
                , _bgpGenStakeholders = gdBootStakeholders genesisData
                , _bgpBlockCount      = fromIntegral bgoBlockN
                , _bgpTxGenParams     = bgoTxGenParams
                , _bgpInplaceDB       = True
                , _bgpSkipNoKey       = True
                }
    liftIO $ bracket (openNodeDBs (not bgoAppend) bgoPath) closeNodeDBs $ \db ->
        runProduction $
        initTBlockGenMode db $
        void $ evalRandT (genBlocks bgenParams) (mkStdGen seed)
    -- We print it twice because there can be a ton of logs and
    -- you don't notice the first message.
    putText $ "Generated with seed " <> show seed
  where
    catchEx :: TBlockGenError -> IO ()
    catchEx e = putText $ sformat ("Error: "%build) e

    parseSecret p = (^. usPrimKey) <$> peekUserSecret p >>= \case
        Nothing -> throwM $ SecretNotFound p
        Just sk -> pure sk

    checkExistence p =
        unlessM (doesDirectoryExist p) $
            throwM AppendToNonexistDB
