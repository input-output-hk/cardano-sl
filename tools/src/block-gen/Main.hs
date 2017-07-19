module Main where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import           Formatting          (build, sformat, (%))
import           Mockable            (runProduction)
import           System.Directory    (doesDirectoryExist)

import           Pos.Core            (StakeDistribution (..), addressHash,
                                      genesisDevKeyPairs, genesisProdAddrDistribution,
                                      isDevelopment, makePubKeyAddress, mkCoin)
import           Pos.Crypto          (SecretKey, toPublic)
import           Pos.DB              (closeNodeDBs, openNodeDBs)
import           Pos.Generator.Block (AllSecrets (..), BlockGenParams (..), genBlocks)
import           Pos.Genesis         (devAddrDistr, genesisUtxo)
import           Pos.Txp.Core        (TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil        (GenesisUtxo (..), Utxo)
import           Pos.Util.UserSecret (peekUserSecret, usPrimKey)

import           Context             (initTBlockGenMode)
import           Error               (TBlockGenError (..))
import           Options             (BlockGenOptions (..), getBlockGenOptions)

main :: IO ()
main = flip catch catchEx $ do
    BlockGenOptions{..} <- getBlockGenOptions
    when bgoAppend $ checkExistence bgoPath
    secretsMap <- case bgoNodes of
        Left bgoNodesN -> do
            let keys = take (fromIntegral bgoNodesN) genesisDevKeyPairs
            let secretsMap = HM.fromList $ map (first addressHash) keys
            pure secretsMap
        Right bgoSecretFiles -> do
            when (null bgoSecretFiles) $
                throwM NoOneSecrets
            secrets <- runProduction $ mapM parseSecret bgoSecretFiles
            let secretsMap = HM.fromList $
                                map (first (addressHash . toPublic) . join (,)) secrets
            pure secretsMap

    let nodes = length secretsMap
    let flatDistr = FlatStakes (fromIntegral nodes) (mkCoin $ fromIntegral nodes)
    let bootStakeholders = HM.fromList $ zip (HM.keys secretsMap) (repeat 1)
    -- We need to select from utxo TxOut's corresponding to passed secrets
    -- to avoid error "Secret key of %hash% is required but isn't known"
    let genUtxo = GenesisUtxo $ filterSecretsUtxo (toList secretsMap) $
            if isDevelopment
            then genesisUtxo Nothing (devAddrDistr flatDistr)
            else genesisUtxo (Just bootStakeholders) genesisProdAddrDistribution
    when (M.null $ unGenesisUtxo genUtxo) $
        throwM EmptyUtxo

    let bgenParams = BlockGenParams (AllSecrets secretsMap) (fromIntegral bgoBlockN) True
    --seed <- maybe randomIO pure bgoSeed
    -- TODO use seed in the future

    bracket (openNodeDBs (not bgoAppend) bgoPath) closeNodeDBs $ \db ->
        runProduction $
        initTBlockGenMode db genUtxo $
        void $ genBlocks bgenParams
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
