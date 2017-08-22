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

import           Pos.AllSecrets              (AllSecrets (..), mkInvAddrSpendingData,
                                              mkInvSecretsMap)
import           Pos.Core                    (AddrSpendingData (..),
                                              IsBootstrapEraAddr (..),
                                              genesisDevSecretKeys, giveStaticConsts,
                                              isDevelopment, makePubKeyAddress,
                                              makePubKeyAddressBoot)
import           Pos.Crypto                  (SecretKey, toPublic)
import           Pos.DB                      (closeNodeDBs, openNodeDBs)
import           Pos.Generator.Block         (BlockGenParams (..), genBlocks)
import           Pos.Genesis                 (GenesisContext (..), devGenesisContext,
                                              genesisContextProduction,
                                              genesisDevFlatDistr, gtcUtxo,
                                              gtcWStakeholders)
import           Pos.Txp.Core                (TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil                (GenesisUtxo (..), Utxo, _GenesisUtxo)
import           Pos.Util.UserSecret         (peekUserSecret, usPrimKey)

import           Context                     (initTBlockGenMode)
import           Error                       (TBlockGenError (..))
import           Options                     (BlockGenOptions (..), getBlockGenOptions)

main :: IO ()
main = flip catch catchEx $ giveStaticConsts $ do
    BlockGenOptions{..} <- getBlockGenOptions
    seed <- maybe randomIO pure bgoSeed
    if isDevelopment then
        putText $ "Generating in DEV mode with seed " <> show seed
    else
        putText $ "Generating in PROD mode with seed " <> show seed

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

    let npGenesisCtx
            | isDevelopment = devGenesisContext genesisDevFlatDistr
            | otherwise = genesisContextProduction

    let bootStakeholders = npGenesisCtx ^. gtcWStakeholders

    -- We need to select from utxo TxOut's corresponding to passed secrets
    -- to avoid error "Secret key of %hash% is required but isn't known"
    let genUtxo = npGenesisCtx ^. gtcUtxo &
                  _GenesisUtxo %~ filterSecretsUtxo (toList invSecretsMap)
    let genCtx = GenesisContext genUtxo bootStakeholders
    when (M.null $ unGenesisUtxo genUtxo) $
        throwM EmptyUtxo

    -- We need to construct an 'InvAddrSpendingData'. We can't be sure
    -- whether bootstrap era will be used or not, so we put addresses
    -- with bootstrap era distribution as well as addresses with
    -- single key distribution. Spending data list is the same.
    let pks = toPublic <$> toList invSecretsMap
    let addressesNonBoot = map (makePubKeyAddress (IsBootstrapEraAddr False)) pks
    let addressesBoot = map makePubKeyAddressBoot pks
    let spendingDataList = map PubKeyASD pks
    let invAddrSpendingData = mkInvAddrSpendingData $
            zip addressesNonBoot spendingDataList <>
            zip addressesBoot spendingDataList

    let bgenParams =
            BlockGenParams
                (AllSecrets invSecretsMap invAddrSpendingData)
                (fromIntegral bgoBlockN)
                def
                True
                bootStakeholders
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

    -- This function is used to filter genesis utxo which always
    -- contains addresses with bootstrap era distribution, hence we
    -- use this distribution here.
    filterSecretsUtxo :: [SecretKey] -> Utxo -> Utxo
    filterSecretsUtxo secrets utxo = do
        let addrs = map (makePubKeyAddressBoot . toPublic) secrets
        let inAddrs x = txOutAddress (toaOut x) `elem` addrs
        M.filter inAddrs utxo

    parseSecret p = (^. usPrimKey) <$> peekUserSecret p >>= \case
        Nothing -> throwM $ SecretNotFound p
        Just sk -> pure sk

    checkExistence p =
        unlessM (doesDirectoryExist p) $
            throwM AppendToNonexistDB
