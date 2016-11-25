module TxGeneration
       ( BambooPool
       , initTransaction
       , createBambooPool
       , curBambooTx
       , peekTx
       , nextValidTx
       , resetBamboo
       ) where

import           Control.TimeWarp.Timed (sec)
import           Data.Array.IO          (IOArray)
import           Data.Array.MArray      (newListArray, readArray, writeArray)
import           Data.IORef             (IORef, modifyIORef', newIORef, readIORef,
                                         writeIORef)
import           Data.List              (tail, (!!))
import           Universum              hiding (head)

import           Pos.Constants          (k, slotDuration)
import           Pos.Crypto             (SecretKey, hash, sign, unsafeHash)
import           Pos.Genesis            (genesisAddresses, genesisSecretKeys)
import           Pos.State              (isTxVerified)
import           Pos.Types              (Address, Tx (..), TxId, TxIn (..), TxOut (..))
import           Pos.WorkMode           (WorkMode)

import           GenOptions             (GenOptions (..))

-- txChain :: Int -> [Tx]
-- txChain i = genChain (genesisSecretKeys !! i) addr (unsafeHash addr) 0
--   where addr = genesisAddresses !! i

tpsTxBound :: Double -> Int -> Int
tpsTxBound tps propThreshold =
    round tps * (k + propThreshold) * fromIntegral (slotDuration `div` sec 1)

genChain :: SecretKey -> Address -> TxId -> Word32 -> [Tx]
genChain secretKey addr txInHash txInIndex =
    let txOutValue = 1
        txOutputs = [TxOut { txOutAddress = addr, ..}]
        txInputs = [TxIn { txInSig = sign secretKey (txInHash, txInIndex, txOutputs), .. }]
        resultTransaction = Tx {..}
    in resultTransaction : genChain secretKey addr (hash resultTransaction) 0

initTransaction :: GenOptions -> Tx
initTransaction GenOptions {..} =
    let maxTps = goInitTps + goTpsIncreaseStep * fromIntegral goRoundNumber
        n' = tpsTxBound maxTps goPropThreshold
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
          bamboos = map (tx :) $
                    map (genChain sk addr (hash tx) . fromIntegral) [0 .. outputsN - 1]

shiftTx :: BambooPool -> IO ()
shiftTx BambooPool {..} = do
    idx <- readIORef bpCurIdx
    chain <- readArray bpChains idx
    writeArray bpChains idx $ tail chain

nextBamboo :: BambooPool -> Double -> Int -> IO ()
nextBamboo BambooPool {..} curTps propThreshold = do
    let bound = tpsTxBound curTps propThreshold
    modifyIORef' bpCurIdx $ \idx ->
        (idx + 1) `mod` bound

resetBamboo :: BambooPool -> IO ()
resetBamboo BambooPool {..} = writeIORef bpCurIdx 0

getTx :: BambooPool -> Int -> Int -> IO Tx
getTx BambooPool {..} bambooIdx txIdx =
    (!! txIdx) <$> readArray bpChains bambooIdx

curBambooTx :: BambooPool -> Int -> IO Tx
curBambooTx bp@BambooPool {..} idx =
    join $ getTx bp <$> readIORef bpCurIdx <*> pure idx

peekTx :: BambooPool -> IO Tx
peekTx bp = curBambooTx bp 0

nextValidTx :: WorkMode ssc m => BambooPool -> Double -> Int -> m (Either Tx Tx)
nextValidTx bp curTps propThreshold = do
    curTx <- liftIO $ curBambooTx bp 1
    isVer <- isTxVerified curTx
    liftIO $ if isVer
        then do
        shiftTx bp
        nextBamboo bp curTps propThreshold
        return $ Right curTx
        else do
        parentTx <- peekTx bp
        nextBamboo bp curTps propThreshold
        return $ Left parentTx
