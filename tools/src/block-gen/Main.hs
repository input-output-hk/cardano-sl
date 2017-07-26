module Main where

import           Universum

import           Control.Monad.Random.Strict (evalRandT)
import           Data.Default                (def)
import qualified Data.HashMap.Strict         as HM
import qualified Data.Map                    as M
import           Formatting                  (build, sformat, (%))
import           Mockable                    (runProduction)
import           System.Directory            (doesDirectoryExist)
import           System.Random               (newStdGen)
import           System.Wlog                 (usingLoggerName)

import           Pos.Core                    (StakeDistribution (..), addressHash,
                                              genesisDevKeyPairs,
                                              genesisProdAddrDistribution, isDevelopment,
                                              makePubKeyAddress, mkCoin)
import           Pos.Crypto                  (SecretKey, toPublic)
import           Pos.DB                      (closeNodeDBs, openNodeDBs)
import           Pos.Generator.Block         (AllSecrets (..), BlockGenParams (..),
                                              genBlocks)
import           Pos.Genesis                 (devAddrDistr, genesisUtxo)
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
    secretsMap <- case bgoNodes of
        Left bgoNodesN -> do
            unless (bgoNodesN > 0) $
                throwM NoOneSecrets
            let keys = take (fromIntegral bgoNodesN) genesisDevKeyPairs
            let secretsMap = HM.fromList $ map (first addressHash) keys
            pure secretsMap
        Right bgoSecretFiles -> do
            when (null bgoSecretFiles) $
                throwM NoOneSecrets
            secrets <- usingLoggerName "block-gen" $ mapM parseSecret bgoSecretFiles
            let secretsMap = HM.fromList $
                                map (first (addressHash . toPublic) . join (,)) secrets
            pure secretsMap

    let nodes = length secretsMap
    let flatDistr = FlatStakes (fromIntegral nodes) (mkCoin $ fromIntegral nodes)
    let bootStakeholders = HM.fromList $ zip (HM.keys secretsMap) (repeat 1)
    -- We need to select from utxo TxOut's corresponding to passed secrets
    -- to avoid error "Secret key of %hash% is required but isn't known"
    let genUtxoUnfiltered
            | isDevelopment = genesisUtxo Nothing (devAddrDistr flatDistr)
            | otherwise =
                 genesisUtxo (Just bootStakeholders) genesisProdAddrDistribution
    let genUtxo = genUtxoUnfiltered &
            _GenesisUtxo %~ filterSecretsUtxo (toList secretsMap)
    when (M.null $ unGenesisUtxo genUtxo) $
        throwM EmptyUtxo

    let bgenParams = BlockGenParams (AllSecrets secretsMap) (fromIntegral bgoBlockN) def True
    --seed <- maybe randomIO pure bgoSeed
    -- TODO use seed in the future
    bracket (openNodeDBs (not bgoAppend) bgoPath) closeNodeDBs $ \db ->
        runProduction $
        initTBlockGenMode db genUtxo $ do
            g <- liftIO newStdGen
            void $ evalRandT (genBlocks bgenParams) g
    -- We print it twice because there can be a ton of logs and
    -- you don't notice the first message.
    if isDevelopment then
        putText $ "Generated in DEV mode"
    else
        putText $ "Generated in PROD mode"
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
