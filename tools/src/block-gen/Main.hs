module Main where

import           Universum

import           Control.Monad.Random.Strict (evalRandT)
import           Data.Default                (def)
import qualified Data.Map                    as M
import           Formatting                  (build, sformat, (%))
import           Mockable                    (runProduction)
import           System.Directory            (doesDirectoryExist)
import           System.Random               (mkStdGen, randomIO)
import           System.Wlog                 (usingLoggerName)

import           Pos.AllSecrets              (mkAllSecretsSimple)
import           Pos.Core                    (genesisDevSecretKeys, giveStaticConsts,
                                              isDevelopment)
import           Pos.DB                      (closeNodeDBs, openNodeDBs)
import           Pos.Generator.Block         (BlockGenParams (..), genBlocks)
import           Pos.Genesis                 (genesisContextProduction, gtcUtxo,
                                              gtcWStakeholders)
import           Pos.Launcher                (applyConfigInfo)
import           Pos.Txp.Toil                (GenesisUtxo (..))
import           Pos.Util.UserSecret         (peekUserSecret, usPrimKey)

import           Context                     (initTBlockGenMode)
import           Error                       (TBlockGenError (..))
import           Options                     (BlockGenOptions (..), getBlockGenOptions)

main :: IO ()
main = (applyConfigInfo def >>) $ flip catch catchEx $ giveStaticConsts $ do
    BlockGenOptions{..} <- getBlockGenOptions
    seed <- maybe randomIO pure bgoSeed
    let runMode = bool "PROD" "DEV" isDevelopment
    putText $ "Generating in " <> runMode <> " mode with seed " <> show seed

    when bgoAppend $ checkExistence bgoPath
    allSecrets <- mkAllSecretsSimple <$> case bgoNodes of
        Left bgoNodesN -> do
            unless (bgoNodesN > 0) $ throwM NoOneSecrets
            pure $ take (fromIntegral bgoNodesN) genesisDevSecretKeys
        Right bgoSecretFiles -> do
            when (null bgoSecretFiles) $ throwM NoOneSecrets
            usingLoggerName "block-gen" $ mapM parseSecret bgoSecretFiles

    let genCtx = genesisContextProduction

    let bootStakeholders = genCtx ^. gtcWStakeholders
    let genUtxo = genCtx ^. gtcUtxo
    when (M.null $ unGenesisUtxo genUtxo) $ throwM EmptyUtxo

    let bgenParams =
            BlockGenParams
                { _bgpSecrets         = allSecrets
                , _bgpGenStakeholders = bootStakeholders
                , _bgpBlockCount      = fromIntegral bgoBlockN
                , _bgpTxGenParams     = bgoTxGenParams
                , _bgpInplaceDB       = True
                , _bgpSkipNoKey       = True
                }
    bracket (openNodeDBs (not bgoAppend) bgoPath) closeNodeDBs $ \db ->
        runProduction $
        initTBlockGenMode db genCtx $
            void $ evalRandT (genBlocks bgenParams) (mkStdGen seed)
    -- We print it twice because there can be a ton of logs and
    -- you don't notice the first message.
    if isDevelopment then
        putText $ "Generated in DEV mode with seed " <> show seed
    else
        putText $ "Generated in PROD mode with seed " <> show seed
  where
    catchEx :: TBlockGenError -> IO ()
    catchEx e = putText $ sformat ("Error: "%build) e

    parseSecret p = (^. usPrimKey) <$> peekUserSecret p >>= \case
        Nothing -> throwM $ SecretNotFound p
        Just sk -> pure sk

    checkExistence p =
        unlessM (doesDirectoryExist p) $
            throwM AppendToNonexistDB
