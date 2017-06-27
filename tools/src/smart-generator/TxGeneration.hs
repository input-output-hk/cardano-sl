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
import           Data.Array.MArray             (newListArray, readArray, writeArray)
import           Data.List                     (tail, (!!))
import qualified Data.List.NonEmpty            as NE
import           Data.Time.Units               (convertUnit)
import           Serokell.Util                 (sec)
import           Universum                     hiding (head)

import           Pos.Client.Txp.Util           (makeMOfNTx, makePubKeyTx)
import           Pos.Constants                 (genesisSlotDuration, slotSecurityParam)
import           Pos.Crypto                    (SecretKey, fakeSigner, hash, toPublic,
                                                unsafeHash)
import           Pos.DB.GState                 (getTxOut)
import           Pos.Genesis                   (genesisAddresses, genesisDevPublicKeys,
                                                genesisDevSecretKeys)
import           Pos.Script                    (Script)
import           Pos.Script.Examples           (multisigValidator)
import           Pos.Txp                       (Tx (..), TxAux (..), TxId, TxIn (..),
                                                TxOut (..), TxOutAux (..))
import           Pos.Types                     (makePubKeyAddress, makeScriptAddress,
                                                mkCoin)
import           Pos.WorkMode.Class            (WorkMode)

import           GenOptions                    (GenOptions (..))

-- txChain :: Int -> [Tx]
-- txChain i = genChain (genesisDevSecretKeys !! i) addr (unsafeHash addr) 0
--   where addr = genesisAddresses !! i

-- TODO: should it use slotSecurityParam or blkSecurityParam?
tpsTxBound :: Double -> Int -> Int
tpsTxBound tps propThreshold =
    round $
    tps * fromIntegral (slotSecurityParam + propThreshold) *
    fromIntegral (convertUnit genesisSlotDuration `div` sec 1)

genChain :: SecretKey -> TxId -> Word32 -> [TxAux]
genChain sk txInHash txInIndex =
    let addr = makePubKeyAddress $ toPublic sk
        ta@(TxAux tx _ _) =
            makePubKeyTx
                (fakeSigner sk)
                (one $ TxIn txInHash txInIndex)
                (one $ TxOutAux (TxOut addr (mkCoin 1)) [])
    in ta : genChain sk (hash tx) 0

genMOfNChain :: Script -> [Maybe SecretKey] -> TxId -> Word32 -> [TxAux]
genMOfNChain val sks txInHash txInIndex =
    let addr = makeScriptAddress val
        ta@(TxAux tx _ _) =
            makeMOfNTx
                val
                (fmap fakeSigner <$> sks)
                (one $ TxIn txInHash txInIndex)
                (one $ TxOutAux (TxOut addr (mkCoin 1)) [])
    in ta : genMOfNChain val sks (hash tx) 0

initTransaction :: GenOptions -> Int -> TxAux
initTransaction GenOptions{..} i =
    let maxTps = goInitTps + goTpsIncreaseStep * fromIntegral goRoundNumber
        n' = tpsTxBound (maxTps / fromIntegral (length goGenesisIdxs)) goPropThreshold
        outputsNum = min n' goInitBalance
        inAddr = genesisAddresses !! i
        sk = genesisDevSecretKeys !! i
        input = TxIn (unsafeHash inAddr) 0
        outAddr = case goMOfNParams of
            Nothing     -> inAddr
            Just (m, n) -> let pks = take n genesisDevPublicKeys
                               val = multisigValidator m pks
                           in makeScriptAddress val
        outputs = replicate outputsNum (TxOutAux (TxOut outAddr (mkCoin 1)) [])
    in makePubKeyTx (fakeSigner sk) (one input) (NE.fromList outputs)

selectSks :: Int -> Int -> [SecretKey] -> [Maybe SecretKey]
selectSks m i sks = permutations msks !! i
  where msks = map Just (take m sks) ++ replicate (length sks - m) Nothing

data BambooPool = BambooPool
    { bpChains :: TArray Int [TxAux]
    , bpCurIdx :: TVar Int
    }

createBambooPool :: Maybe (Int, Int) -> Int -> TxAux -> IO BambooPool
createBambooPool mOfN i ta@(TxAux tx _ _) = atomically $
    BambooPool
        <$> newListArray (0, outputsN - 1) bamboos
        <*> newTVar 0
  where
    outputsN = length (_txOutputs tx)
    sk       = genesisDevSecretKeys !! i
    bamboos  = [ ta : mkChain (fromIntegral j)
               | j <- [0 .. outputsN - 1] ]
    txId     = hash tx
    mkChain  =
        case mOfN of
            Nothing     -> genChain sk txId
            Just (m, n) ->
                let pks = take n genesisDevPublicKeys
                    val = multisigValidator m pks
                    msks = selectSks m i $ take n genesisDevSecretKeys
                in genMOfNChain val msks txId

shiftTx :: BambooPool -> IO ()
shiftTx BambooPool {..} = atomically $ do
    idx <- readTVar bpCurIdx
    chain <- readArray bpChains idx
    writeArray bpChains idx $ tail chain

nextBamboo :: BambooPool -> Double -> Int -> IO ()
nextBamboo BambooPool {..} curTps propThreshold = atomically $ do
    let bound = tpsTxBound curTps propThreshold
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
isTxVerified tx = allM (fmap isJust . getTxOut) $ toList (_txInputs tx)

nextValidTx
    :: WorkMode ssc m
    => BambooPool
    -> Double
    -> Int
    -> m (Either TxAux TxAux)
nextValidTx bp curTps propThreshold = do
    curTx <- liftIO $ curBambooTx bp 1
    isVer <- isTxVerified $ taTx curTx
    liftIO $
        if isVer
        then do
            shiftTx bp
            nextBamboo bp curTps propThreshold
            return $ Right curTx
        else do
            parentTx <- peekTx bp
            nextBamboo bp curTps propThreshold
            return $ Left parentTx
