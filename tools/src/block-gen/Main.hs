module Main where

import           Universum

import           Mockable            (runProduction)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import           Pos.Core            (StakeDistribution (..), addressHash,
                                      genesisDevKeyPairs, makePubKeyAddress, mkCoin)
import           Pos.Crypto          (toPublic, unsafeHash)
import           Pos.DB              (closeNodeDBs, openNodeDBs)
import           Pos.Generator.Block (AllSecrets (..), BlockGenParams (..), genBlocks)
import           Pos.Genesis         (stakeDistribution)
import           Pos.Txp.Core        (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil        (GenesisUtxo (..))
import           System.Directory    (doesDirectoryExist)

import           Context             (initTBlockGenMode)
import           Options             (BlockGenOptions (..), getBlockGenOptions)

main :: IO ()
main = do
    BlockGenOptions{..} <- getBlockGenOptions
    when bgoAppend $ checkExistence bgoPath
    let keys = take (fromIntegral bgoNodesN) genesisDevKeyPairs
    let dummyDistr = FlatStakes (fromIntegral bgoNodesN) (mkCoin $ fromIntegral bgoNodesN)
    let secretsMap = HM.fromList $ map (first addressHash) keys
    let bgenParams = BlockGenParams (AllSecrets secretsMap) (fromIntegral bgoBlockN) True
    let genUtxo =
            GenesisUtxo . M.fromList $
            zipWith
                zipF
                (toList secretsMap)
                (stakeDistribution dummyDistr)

    bracket (openNodeDBs (not bgoAppend) bgoPath) closeNodeDBs $ \db ->
        runProduction $
        initTBlockGenMode db genUtxo $
        void $ genBlocks bgenParams
  where
    -- Copy-pasted from tests.
    zipF secretKey (coin, toaDistr) =
        let addr = makePubKeyAddress (toPublic secretKey)
            toaOut = TxOut addr coin
        in (TxIn (unsafeHash addr) 0, TxOutAux {..})
    checkExistence p =
        unlessM (doesDirectoryExist p) $
            error "You specified --append flag, but DB doesn't exist"
