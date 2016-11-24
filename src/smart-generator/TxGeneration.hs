module TxGeneration
       ( BambooPool
       , initTransaction
       , createBambooPool
       , peekTx
       , nextValidTx
       ) where

import           Control.TimeWarp.Timed (for, ms, sec, wait)
import           Data.Array.IO          (IOArray)
import           Data.Array.MArray      (MArray (..), newListArray, readArray, writeArray)
import           Data.IORef             (IORef, modifyIORef', newIORef, readIORef)
import           Data.List              (head, tail, (!!))
import           Formatting             (sformat, (%))
import           System.Wlog            (logInfo)
import           Universum              hiding (head)

import           Pos.Constants          (k, slotDuration)
import           Pos.Crypto             (SecretKey, hash, sign, unsafeHash)
import           Pos.Genesis            (genesisAddresses, genesisSecretKeys)
import           Pos.State              (isTxVerified)
import           Pos.Types              (Address, Tx (..), TxId, TxIn (..), TxOut (..),
                                         txF)
import           Pos.WorkMode           (WorkMode)

import           GenOptions             (GenOptions (..))

-- txChain :: Int -> [Tx]
-- txChain i = genChain (genesisSecretKeys !! i) addr (unsafeHash addr) 0
--   where addr = genesisAddresses !! i

genChain :: SecretKey -> Address -> TxId -> Word32 -> [Tx]
genChain secretKey addr txInHash txInIndex =
    let txOutValue = 1
        txOutputs = [TxOut { txOutAddress = addr, ..}]
        txInputs = [TxIn { txInSig = sign secretKey (txInHash, txInIndex, txOutputs), .. }]
        resultTransaction = Tx {..}
    in resultTransaction : genChain secretKey addr (hash resultTransaction) 0

initTransaction :: GenOptions -> Tx
initTransaction GenOptions {..} =
    let maxTps = round $ goInitTps + goTpsIncreaseStep * fromIntegral goRoundNumber
        n' = maxTps * (k + goPropThreshold) * fromIntegral (slotDuration `div` sec 1)
        n = min n' goInitBalance
        i = fromIntegral goGenesisIdx
        txOutAddress = genesisAddresses !! i
        secretKey = genesisSecretKeys !! i
        txOutValue = 1
        txOutputs = replicate n (TxOut {..})
        txInHash = unsafeHash txOutAddress
        txInIndex = 0
        txInputs = [TxIn { txInSig = sign secretKey (txInHash, txInIndex, txOutputs), .. }]
    in Tx {..}

data BambooPool = BambooPool
    { bpChains :: IOArray Int [Tx]
    , bpCurIdx :: IORef Int
    }

createBambooPool :: SecretKey -> Address -> Tx -> IO BambooPool
createBambooPool sk addr tx = BambooPool <$> newListArray (0, outputsN - 1) bamboos <*> newIORef 0
    where outputsN = length $ txOutputs tx
          bamboos = --map (tx :) $
                    map (genChain sk addr (hash tx) . fromIntegral) [0 .. outputsN - 1]

shiftTx :: BambooPool -> IO ()
shiftTx BambooPool {..} = do
    idx <- readIORef bpCurIdx
    chain <- readArray bpChains idx
    writeArray bpChains idx $ tail chain

nextBamboo :: BambooPool -> IO ()
nextBamboo BambooPool {..} = do
    lastChainIdx <- snd <$> getBounds bpChains
    modifyIORef' bpCurIdx $ \idx ->
        (idx + 1) `mod` (lastChainIdx + 1)

peekTx :: BambooPool -> IO Tx
peekTx BambooPool {..} =
    readIORef bpCurIdx >>=
    fmap head . readArray bpChains

nextValidTx :: WorkMode ssc m => BambooPool -> Int -> m Tx
nextValidTx bp tpsDelta = do
    tx <- liftIO $ peekTx bp
    isVer <- isTxVerified tx
    if isVer
        then liftIO $ do
        shiftTx bp
        nextBamboo bp
        return tx
        else do
        logInfo $ sformat ("Transaction "%txF%"is not verified yet!") tx
        liftIO $ nextBamboo bp
        wait $ for $ ms tpsDelta
        nextValidTx bp tpsDelta
