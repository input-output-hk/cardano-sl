{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import           Control.Applicative        (empty)
import           Data.Aeson                 (decode, fromJSON, json')
import qualified Data.Aeson                 as A
import           Data.Attoparsec.ByteString (eitherResult, many', parseWith)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.HashMap.Strict        as HM
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Data.Time.Units            (Millisecond)
import           Formatting                 (fixed, int, sformat, shown, string, (%))
import           Options.Applicative.Simple (simpleOptions)
import           Universum
import           Unsafe                     (unsafeFromJust)

import           AnalyzerOptions            (Args (..), argsParser)
import           Pos.Types                  (flattenSlotId, unflattenSlotId)
import           Pos.Util.JsonLog           (JLBlock (..), JLEvent (..),
                                             JLTimedEvent (..), fromJLSlotId)

type TxId = Text
type BlockId = Text

main :: IO ()
main = do
    (Args {..}, ()) <-
        simpleOptions
            "cardano-analyzer"
            "PoS prototype log analyzer"
            "Use it!"
            argsParser
            empty
    logs <- parseFiles files

    case txFile of
        Nothing   -> pure ()
        Just file -> analyzeVerifyTimes file confirmationParam logs

    let tpsLogs :: HM.HashMap FilePath [(UTCTime, Double)]
        tpsLogs = getTpsLog <$> logs

    for_ (HM.toList tpsLogs) $ \(file, ds) -> do
        let csvFile = tpsCsvFilename file
        putText $ sformat ("Writing TPS stats to file: "%string) csvFile
        writeFile csvFile $ tpsToCsv ds

analyzeVerifyTimes :: FilePath -> Word64 -> HM.HashMap FilePath [JLTimedEvent] -> IO ()
analyzeVerifyTimes txFile cParam logs = do
    (txSenderMap :: HashMap TxId Integer) <-
        HM.fromList . fromMaybe (error "failed to read txSenderMap") . decode <$>
        LBS.readFile txFile
    let txConfTimes :: HM.HashMap TxId Integer
        txConfTimes = getTxAcceptTimeAvgs cParam logs
        common =
            HM.intersectionWith (-) txConfTimes txSenderMap
        average :: Double
        average =
            fromIntegral (sum (toList common)) / fromIntegral (length common)
        averageMsec :: Millisecond
        averageMsec = fromInteger . round $ average / 1000
    print $
        sformat ("Number of transactions which are sent and accepted: " %int) $
        length common
    print averageMsec

getTxAcceptTimeAvgs :: Word64 -> HM.HashMap FilePath [JLTimedEvent] -> HM.HashMap TxId Integer
getTxAcceptTimeAvgs confirmations fileEvsMap = result
  where
    n = HM.size fileEvsMap
    allEvs = map jlEvent $ mconcat $ HM.elems fileEvsMap
    blocks :: HM.HashMap BlockId JLBlock
    blocks = foldl' addBlock mempty allEvs
    adopted :: HM.HashMap BlockId (HM.HashMap FilePath Integer)
    adopted = HM.foldlWithKey' adPerFile mempty fileEvsMap
    adPerFile m fp = foldl' (addAdopted fp) m . reverse

    adoptedAvgs :: HM.HashMap BlockId Integer
    adoptedAvgs = maximum <$> HM.filter (\m -> HM.size m >= n `div` 2) adopted
    -- avg = (\m -> sum m `div` fromIntegral (HM.size m))

    result :: HM.HashMap TxId Integer
    result = HM.foldlWithKey' impl mempty adoptedAvgs
      where
        impl m id time = case jlTxs <$> kDepth id of
                           Just txs -> foldl' (addTx time) m txs
                           _        -> m
        addTx time m tx = HM.insert tx time m


    kDepth :: BlockId -> Maybe JLBlock
    kDepth initId |isJust mInitB = impl initId
                  |otherwise     = Nothing
      where
        mInitB = initId `HM.lookup` blocks
        kSl = unflattenSlotId $ flattenSlotId (fromJLSlotId $ jlSlot $ unsafeFromJust mInitB) - confirmations
        impl id = HM.lookup id blocks >>=
                      \b -> if (fromJLSlotId $ jlSlot b) <= kSl
                               then return b
                               else impl (jlPrevBlock b)

    addAdopted fp m (JLTimedEvent time (JLAdoptedBlock blockId)) = HM.insert blockId sm' m
      where
        sm = fromMaybe mempty $ HM.lookup blockId m
        sm' = HM.insert fp time sm
    addAdopted _ m _ = m

    addBlock m (JLCreatedBlock block) = HM.insert (jlHash block) block m
    addBlock m _                      = m

parseFiles :: [FilePath] -> IO (HM.HashMap FilePath [JLTimedEvent])
parseFiles = foldM (\m f -> flip (HM.insert f) m <$> parseFile f) mempty

parseFile :: FilePath -> IO [JLTimedEvent]
parseFile f = do
    bytes <- BS.readFile f
    res <- parseWith (pure mempty) (many' $ json' >>= fromJSON') bytes
    case eitherResult res of
      Left s    -> fail $ "Failed reading file " ++ f ++ ":" ++ s
      Right evs -> return evs
  where
    fromJSON' val = case fromJSON val of
                      A.Error e   -> fail e
                      A.Success a -> return a

tpsCsvFilename :: FilePath -> FilePath
tpsCsvFilename file = take (length file - 5) file ++ "-tps.csv"

getTpsLog :: [JLTimedEvent] -> [(UTCTime, Double)]
getTpsLog = map toTimedCount . filter isTpsEvent
  where isTpsEvent ev = case jlEvent ev of
            JLTpsStat _ -> True
            _           -> False
        toTimedCount (JLTimedEvent time (JLTpsStat count)) =
            ( posixSecondsToUTCTime $ fromIntegral $ time `div` 1000000
            , fromIntegral count
            )
        toTimedCount _ = error "getTpsLog: not TPS stat is given!"

tpsToCsv :: [(UTCTime, Double)] -> Text
tpsToCsv entries =
    "time,tps\n" <>
    mconcat (map formatter entries)
  where
    formatter (time, tps) = sformat (shown%","%fixed 2%"\n") time tps
