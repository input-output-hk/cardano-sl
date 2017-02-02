module TxGeneration
       ( BambooPool
       , initTransaction
       , createBambooPool
       , curBambooTx
       , peekTx
       , nextValidTx
       , resetBamboo
       , isTxVerified
       ) where

import           Control.Concurrent.STM.TArray (TArray)
import           Control.Concurrent.STM.TVar   (TVar, modifyTVar', newTVar, readTVar,
                                                writeTVar)
import           Data.Array.MArray             (newListArray, readArray, writeArray)
import           Data.List                     (tail, (!!))
import           Data.Time.Units               (Millisecond, convertUnit)
import           Universum                     hiding (head)

import           Pos.Constants                 (genesisSlotDuration, slotSecurityParam)
import           Pos.Crypto                    (SecretKey, hash, toPublic, unsafeHash)
import           Pos.DB.GState                 (getTxOut)
import           Pos.Genesis                   (genesisAddresses, genesisPublicKeys,
                                                genesisSecretKeys)
import           Pos.Script                    (Script)
import           Pos.Script.Examples           (multisigValidator)
import           Pos.Types                     (Tx (..), TxAux, TxId, TxOut (..),
                                                makePubKeyAddress, makeScriptAddress,
                                                mkCoin)
import           Pos.Util.TimeWarp             (sec)
import           Pos.Wallet                    (makeMOfNTx, makePubKeyTx)
import           Pos.WorkMode                  (WorkMode)

import           GenOptions                    (GenOptions (..))

-- txChain :: Int -> [Tx]
-- txChain i = genChain (genesisSecretKeys !! i) addr (unsafeHash addr) 0
--   where addr = genesisAddresses !! i

-- TODO: should it use slotSecurityParam or blkSecurityParam?
tpsTxBound :: Millisecond -> Double -> Int -> Int
tpsTxBound slotDuration tps propThreshold =
    round $
    tps * fromIntegral (slotSecurityParam + propThreshold) *
    fromIntegral (convertUnit slotDuration `div` sec 1)

genChain :: SecretKey -> TxId -> Word32 -> [TxAux]
genChain sk txInHash txInIndex =
    let addr = makePubKeyAddress $ toPublic sk
        (tx, w, d) = makePubKeyTx sk [(txInHash, txInIndex)]
                                     [(TxOut addr (mkCoin 1), [])]
    in (tx, w, d) : genChain sk (hash tx) 0

genMOfNChain :: Script -> [Maybe SecretKey] -> TxId -> Word32 -> [TxAux]
genMOfNChain val sks txInHash txInIndex =
    let addr = makeScriptAddress val
        (tx, w, d) = makeMOfNTx val sks [(txInHash, txInIndex)]
                     [(TxOut addr (mkCoin 1), [])]
    in (tx, w, d) : genMOfNChain val sks (hash tx) 0

initTransaction :: GenOptions -> Millisecond -> Int -> TxAux
initTransaction GenOptions{..} slotDuration i =
    let maxTps = goInitTps + goTpsIncreaseStep * fromIntegral goRoundNumber
        n' = tpsTxBound slotDuration (maxTps / fromIntegral (length goGenesisIdxs)) goPropThreshold
        outputsNum = min n' goInitBalance
        inAddr = genesisAddresses !! i
        sk = genesisSecretKeys !! i
        input = (unsafeHash inAddr, 0)
        outAddr = case goMOfNParams of
            Nothing     -> inAddr
            Just (m, n) -> let pks = take n genesisPublicKeys
                               val = multisigValidator m pks
                           in makeScriptAddress val
        outputs = replicate outputsNum (TxOut outAddr (mkCoin 1), [])
    in makePubKeyTx sk [input] outputs

selectSks :: Int -> Int -> [SecretKey] -> [Maybe SecretKey]
selectSks m i sks = permutations msks !! i
  where msks = map Just (take m sks) ++ replicate (length sks - m) Nothing

data BambooPool = BambooPool
    { bpChains :: TArray Int [TxAux]
    , bpCurIdx :: TVar Int
    }

createBambooPool :: Maybe (Int, Int) -> Int -> TxAux -> IO BambooPool
createBambooPool mOfN i (tx, w, d) = atomically $ BambooPool <$> newListArray (0, outputsN - 1) bamboos <*> newTVar 0
    where outputsN = length $ txOutputs tx
          sk = genesisSecretKeys !! i
          bamboos = map (((tx, w, d) :) . mkChain . fromIntegral) [0 .. outputsN - 1]
          txId = hash tx
          mkChain = case mOfN of
              Nothing     -> genChain sk txId
              Just (m, n) -> let pks = take n genesisPublicKeys
                                 val = multisigValidator m pks
                                 msks = selectSks m i $ take n genesisSecretKeys
                             in genMOfNChain val msks txId

shiftTx :: BambooPool -> IO ()
shiftTx BambooPool {..} = atomically $ do
    idx <- readTVar bpCurIdx
    chain <- readArray bpChains idx
    writeArray bpChains idx $ tail chain

nextBamboo :: BambooPool -> Millisecond -> Double -> Int -> IO ()
nextBamboo BambooPool {..} slotDuration curTps propThreshold = atomically $ do
    let bound = tpsTxBound slotDuration curTps propThreshold
    modifyTVar' bpCurIdx $ \idx ->
        (idx + 1) `mod` bound

resetBamboo :: BambooPool -> IO ()
resetBamboo BambooPool {..} = atomically $ writeTVar bpCurIdx 0

getTx :: BambooPool -> Int -> Int -> STM TxAux
getTx BambooPool {..} bambooIdx txIdx =
    (!! txIdx) <$> readArray bpChains bambooIdx

curBambooTx :: BambooPool -> Int -> IO TxAux
curBambooTx bp@BambooPool {..} idx = atomically $
    join $ getTx bp <$> readTVar bpCurIdx <*> pure idx

peekTx :: BambooPool -> IO TxAux
peekTx bp = curBambooTx bp 0

isTxVerified :: (WorkMode ssc m) => Tx -> m Bool
isTxVerified tx = allM (fmap isJust . getTxOut) (txInputs tx)

nextValidTx
    :: WorkMode ssc m
    => BambooPool
    -> Double
    -> Int
    -> m (Either TxAux TxAux)
nextValidTx bp curTps propThreshold = do
    curTx <- liftIO $ curBambooTx bp 1
    isVer <- isTxVerified $ view _1 curTx
    let slotDuration = genesisSlotDuration
    undefined
    liftIO $
        if isVer
        then do
            shiftTx bp
            nextBamboo bp slotDuration curTps propThreshold
            return $ Right curTx
        else do
            parentTx <- peekTx bp
            nextBamboo bp slotDuration curTps propThreshold
            return $ Left parentTx
