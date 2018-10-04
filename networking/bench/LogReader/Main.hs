{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Applicative (empty)
import           Control.Exception.Safe (throwString)
import           Control.Lens (at, (^.))
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Conduit (ConduitT, Void, runConduit, yield, (.|))
import           Data.Conduit.Binary (sinkFile, sourceFile)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Text (decode, encode, utf8)
import           Data.Foldable (foldrM)
import           Data.List (intersperse)
import qualified Data.Map as M
import           Data.Text (Text)
import           Formatting (bprint, int, right, sformat, (%))
import qualified Formatting as F
import           System.IO (FilePath)

import           Data.Attoparsec.Text (parseOnly)
import           Options.Applicative.Simple (simpleOptions)

import           Bench.Network.Commons (LogMessage (..), MeasureEvent (..),
                     MeasureInfo (..), MsgId, Payload (..), Timestamp,
                     logMessageParser, measureInfoParser)
import           LogReaderOptions (Args (..), argsParser)
import           Pos.Util.Trace (Severity (..), Trace, traceWith, wlogTrace)
import           System.Wlog (productionB, setupLogging)
import           System.Wlog.Formatter (centiUtcTimeF)


type Measures = M.Map MsgId (Payload, [(MeasureEvent, Timestamp)])

type RowId = Int

analyze :: Trace IO (Severity, Text) -> FilePath -> Measures -> IO Measures
analyze logTrace file initialMeasures = runResourceT $ pipeline

  where

    pipelineSource :: ConduitT () (Text, RowId) (ResourceT IO) ()
    pipelineSource =
           sourceFile file
        .| CB.lines
        .| decode utf8
        .| (CL.mapAccum withRowId 0 >> pure ())

    pipelineSink :: ConduitT (Text, RowId) Void (ResourceT IO) Measures
    pipelineSink = CL.foldM saveMeasure initialMeasures

    pipeline :: ResourceT IO Measures
    pipeline = runConduit $ pipelineSource .| pipelineSink

    withRowId :: Text -> RowId -> (RowId, (Text, RowId))
    withRowId t rowid = let !rowid' = rowid + 1 in (rowid', (t, rowid))

    saveMeasure :: Measures -> (Text, RowId) -> ResourceT IO Measures
    saveMeasure !measures (row, rowid) = case parseOnly (logMessageParser measureInfoParser) row of
        Left err -> do
            liftIO $ traceWith logTrace $ (,) Warning $
                sformat ("Parse error at file "%F.build%" (line "%F.int%"): "%F.build)
                file rowid err
            pure measures
        Right (Just (LogMessage MeasureInfo{..})) ->
            let alteration Nothing                    = Just (miPayload, [(miEvent, miTime)])
                alteration (Just (miPayload', stuff)) = Just (miPayload', (miEvent, miTime) : stuff)
                measures'  = M.alter alteration miId measures
            in  pure measures'
        Right _ -> pure measures

printMeasures :: FilePath -> Measures -> IO ()
printMeasures file measures = runResourceT . runConduit $
    source .| encode utf8 .| sinkFile file
  where
    source = printHeader >> mapM_ printMeasure (M.toList measures)

    printHeader = printRow $ "MsgId" : "Size" : map (sformat F.build) eventsUniverse

    printMeasure :: Monad m
                 => (MsgId, (Payload, [(MeasureEvent, Timestamp)])) -> ConduitT () Text m ()
    printMeasure (mid, (Payload p, mm)) = do
        case uniqMap mm of
            Just mm' -> printRow $ sformat int mid
                          : sformat int p
                          : [ maybe "-" (sformat int) $ mm' ^. at ev | ev <- eventsUniverse ]
            _ -> return ()

    printRow :: Monad m => [Text] -> ConduitT () Text m ()
    printRow = yield
             . sformat (F.build%"\n")
             . mconcat
             . intersperse ","
             . alignColumns

    uniqMap = foldl upd (Just mempty)
      where
        upd m (ev, ts) = m >>= \m' ->
            case ev `M.lookup` m' of
              Nothing -> return $ M.insert ev ts m'
              _       -> throwString ""

    alignColumns = map (\(s, m) -> bprint (right s ' ') m)
                  . zip (7 : 7 : (18 <$ eventsUniverse))

    eventsUniverse = [minBound .. maxBound]

getOptions :: IO Args
getOptions = (\(a, ()) -> a) <$> simpleOptions
    "bench-log-reader"
    "Utility to extract measures from logs into csv file"
    "Use it!"
    argsParser
    empty

main :: IO ()
main = do
    setupLogging (Just centiUtcTimeF) productionB
    let logTrace = wlogTrace mempty
    Args{..} <- liftIO getOptions
    measures <- foldrM (analyze logTrace) M.empty inputFiles
    printMeasures resultFile measures
