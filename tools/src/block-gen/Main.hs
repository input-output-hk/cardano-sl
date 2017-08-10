module Main where

import           Universum

import           Control.Monad.Random.Strict (evalRandT)
import           Data.Default                (def)
import qualified Data.HashMap.Strict         as HM
import qualified Data.Map                    as M
import           Formatting                  (build, sformat, (%))
import           Mockable                    (runProduction)
import           System.Directory            (doesDirectoryExist)
import           System.Random               (mkStdGen, randomIO)
import           System.Wlog                 (usingLoggerName)

import           Pos.Core                    (StakeDistribution (..),
                                              genesisDevSecretKeys,
                                              genesisProdAddrDistribution, isDevelopment,
                                              makePubKeyAddress, mkCoin)
import           Pos.Crypto                  (SecretKey, toPublic)
import           Pos.DB                      (closeNodeDBs, openNodeDBs)
import           Pos.Generator.Block         (AllSecrets (..), BlockGenParams (..),
                                              genBlocks, mkInvSecretsMap, unInvSecretsMap)
import           Pos.Genesis                 (GenesisWStakeholders (..), devAddrDistr,
                                              genesisUtxo)
import           Pos.Txp.Core                (TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil                (GenesisUtxo (..), Utxo, _GenesisUtxo)
import           Pos.Util.UserSecret         (peekUserSecret, usPrimKey)

import           Context                     (initTBlockGenMode)
import           Error                       (TBlockGenError (..))
import           Options                     (BlockGenOptions (..), getBlockGenOptions)

main :: IO ()
main = flip catch catchEx $ do
    if isDevelopment then
        putText $ "Generating in DEV mode"
    else
        putText $ "Generating in PROD mode"
    BlockGenOptions{..} <- getBlockGenOptions
    when bgoAppend $ checkExistence bgoPath
    invSecretsMap <- mkInvSecretsMap <$> case bgoNodes of
        Left bgoNodesN -> do
            unless (bgoNodesN > 0) $
                throwM NoOneSecrets
            let secretKeys = take (fromIntegral bgoNodesN) genesisDevSecretKeys
            pure secretKeys
        Right bgoSecretFiles -> do
            when (null bgoSecretFiles) $
                throwM NoOneSecrets
            usingLoggerName "block-gen" $ mapM parseSecret bgoSecretFiles

    let nodes = length invSecretsMap
    let flatDistr = FlatStakes (fromIntegral nodes) (mkCoin $ fromIntegral nodes)
    let bootStakeholders =
            GenesisWStakeholders $ HM.fromList $
            zip (HM.keys $ unInvSecretsMap invSecretsMap) (repeat 1)
    -- We need to select from utxo TxOut's corresponding to passed secrets
    -- to avoid error "Secret key of %hash% is required but isn't known"
    let genUtxoUnfiltered
            | isDevelopment = genesisUtxo bootStakeholders (fst $ devAddrDistr flatDistr)
            | otherwise = genesisUtxo bootStakeholders genesisProdAddrDistribution
    let genUtxo = genUtxoUnfiltered &
            _GenesisUtxo %~ filterSecretsUtxo (toList invSecretsMap)
    when (M.null $ unGenesisUtxo genUtxo) $
        throwM EmptyUtxo

    let bgenParams =
            BlockGenParams
                (AllSecrets invSecretsMap)
                (fromIntegral bgoBlockN)
                def
                True
                bootStakeholders
    seed <- maybe randomIO pure bgoSeed
    bracket (openNodeDBs (not bgoAppend) bgoPath) closeNodeDBs $ \db ->
        runProduction $
        initTBlockGenMode db genUtxo $
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

    filterSecretsUtxo :: [SecretKey] -> Utxo -> Utxo
    filterSecretsUtxo secrets utxo = do
        let addrs = map (makePubKeyAddress . toPublic) secrets
        let inAddrs x = txOutAddress (toaOut x) `elem` addrs
        M.filter inAddrs utxo

    parseSecret p = (^. usPrimKey) <$> peekUserSecret p >>= \case
        Nothing -> throwM $ SecretNotFound p
        Just sk -> pure sk

    checkExistence p =
        unlessM (doesDirectoryExist p) $
            throwM AppendToNonexistDB
