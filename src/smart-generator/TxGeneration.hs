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
import qualified Data.Vector            as V
import           Universum              hiding (head)

import           Pos.Constants          (k, slotDuration)
import           Pos.Crypto             (SecretKey, hash, sign, unsafeHash)
import           Pos.Genesis            (genesisAddresses, genesisSecretKeys)
import           Pos.State              (isTxVerified)
import           Pos.Types              (Address, Tx (..), TxId, TxIn (..), TxOut (..),
                                         TxWitness (..))
import           Pos.WorkMode           (WorkMode)

import           GenOptions             (GenOptions (..))

-- txChain :: Int -> [Tx]
-- txChain i = genChain (genesisSecretKeys !! i) addr (unsafeHash addr) 0
--   where addr = genesisAddresses !! i

tpsTxBound :: Double -> Int -> Int
tpsTxBound tps propThreshold =
    round tps * (k + propThreshold) * fromIntegral (slotDuration `div` sec 1)

genChain :: SecretKey -> Address -> TxId -> Word32 -> [(Tx, TxWitness)]
genChain secretKey addr txInHash txInIndex =
    let txOutValue = 1
        txOutputs = [TxOut { txOutAddress = addr, ..}]
        txInputs = [TxIn { .. }]
        resultTransaction = Tx {..}
        resultWitness = V.fromList [
            sign secretKey (txInHash, txInIndex, txOutputs) ]
    in (resultTransaction, resultWitness) :
       genChain secretKey addr (hash resultTransaction) 0

initTransaction :: GenOptions -> (Tx, TxWitness)
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
        txInputs = [TxIn { .. }]
        txWitness = V.fromList [
            sign secretKey (txInHash, txInIndex, txOutputs) ]
    in (Tx {..}, txWitness)

data BambooPool = BambooPool
    { bpChains :: IOArray Int [(Tx, TxWitness)]
    , bpCurIdx :: IORef Int
    }

createBambooPool :: SecretKey -> Address -> (Tx, TxWitness) -> IO BambooPool
createBambooPool sk addr (tx,w) =
    BambooPool <$> newListArray (0, outputsN - 1) bamboos <*> newIORef 0
  where outputsN = length $ txOutputs tx
        bamboos = map ((tx,w) :) $
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

getTx :: BambooPool -> Int -> Int -> IO (Tx, TxWitness)
getTx BambooPool {..} bambooIdx txIdx =
    (!! txIdx) <$> readArray bpChains bambooIdx

curBambooTx :: BambooPool -> Int -> IO (Tx, TxWitness)
curBambooTx bp@BambooPool {..} idx =
    join $ getTx bp <$> readIORef bpCurIdx <*> pure idx

peekTx :: BambooPool -> IO (Tx, TxWitness)
peekTx bp = curBambooTx bp 0

nextValidTx
    :: WorkMode ssc m
    => BambooPool
    -> Double
    -> Int
    -> m (Either (Tx, TxWitness) (Tx, TxWitness))
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

