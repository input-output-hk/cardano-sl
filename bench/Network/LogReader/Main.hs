{-# LANGUAGE TypeApplications #-}

import           Control.Applicative          (empty, (<|>))
import           Control.Exception            (Exception)
import           Control.Lens                 (at, (%=), (^.), _2,
                                               _Just)
import           Control.Monad                (forM_)
import           Control.Monad.Catch          (handle)
import           Control.Monad.State          (StateT (..), evalStateT, execStateT, get,
                                               modify)
import           Control.Monad.Trans          (lift, liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit                 (Source, yield, ($$), (=$=))
import           Data.Conduit.Binary          (sinkFile, sourceFile)
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Data.Conduit.Text            (decode, encode, utf8)
import           Data.List                    (intersperse)
import qualified Data.Map                     as M
import           Data.Text                    (Text)
import           Data.Text.Buildable          (Buildable (..))
import           Data.Typeable                (Typeable)
import           Formatting                   (bprint, int, right, sformat, (%))
import qualified Formatting                   as F
import           System.IO                    (FilePath)

import           Data.Attoparsec.Text         (parseOnly)
import           Options.Applicative.Simple   (simpleOptions)

import           Bench.Network.Commons        (LogMessage (..), MeasureEvent (..),
                                               MeasureInfo (..), MsgId, Payload (..),
                                               Timestamp, logMessageParser,
                                               measureInfoParser)
import           LogReaderOptions             (Args (..), argsParser)
import           System.Wlog                  (LoggerNameBox, Severity (Info),
                                               initLogging, logError, logWarning,
                                               usingLoggerName, usingLoggerName)


type Measures = M.Map MsgId (Payload, [(MeasureEvent, Timestamp)])

newtype MeasureInfoDuplicateError = MeasureInfoDuplicateError (Timestamp, MeasureInfo)
    deriving (Typeable)

instance Show MeasureInfoDuplicateError where
    show = F.formatToString F.build . build

instance Buildable MeasureInfoDuplicateError where
    build (MeasureInfoDuplicateError (was, new)) = mconcat
        ["Duplicate measure: was "
        , build was
        , " but meet "
        , build new
        ]

instance Exception MeasureInfoDuplicateError


type RowId = Int

analyze :: FilePath -> StateT Measures (LoggerNameBox IO) ()
analyze file =
    catchE . flip evalStateT 0 . runResourceT $
        sourceFile file =$= CB.lines =$= CL.iterM (const $ modify succ)
            =$= decode utf8 $$ CL.mapM_ (lift . saveMeasure)
  where
    saveMeasure :: Text -> StateT RowId (StateT Measures (LoggerNameBox IO)) ()
    saveMeasure row = do
        case parseOnly (logMessageParser measureInfoParser) row of
            Left err -> do
                rowNo <- get
                logWarning $
                    sformat ("Parse error at file "%F.build%" (line "%F.int%"): "%F.build)
                    file rowNo err
            Right (Just (LogMessage MeasureInfo{..})) -> lift $ do
                at miId %= (<|> Just (miPayload, mempty))
                at miId . _Just . _2 %= ((miEvent, miTime):)
                --mwas <- singular (at miId . _Just . _2) . at miEvent <<.= Just miTime
                --forM_ mwas $ \was -> throwM $ MeasureInfoDuplicateError (was, mi)
            Right _ -> return ()

    catchE = handle @_ @MeasureInfoDuplicateError $ logError . sformat F.build


printMeasures :: FilePath -> Measures -> LoggerNameBox IO ()
printMeasures file measures = runResourceT $
    source $$ encode utf8 =$= sinkFile file
  where
    source = printHeader >> mapM_ printMeasure (M.toList measures)

    printHeader = printRow $ "MsgId" : "Size" : map (sformat F.build) eventsUniverse

    printMeasure :: Monad m
                 => (MsgId, (Payload, [(MeasureEvent, Timestamp)])) -> Source m Text
    printMeasure (mid, (Payload p, mm)) = do
        case uniqMap mm of
            Just mm' -> printRow $ sformat int mid
                          : sformat int p
                          : [ maybe "-" (sformat int) $ mm' ^. at ev | ev <- eventsUniverse ]
            _ -> return ()

    printRow :: Monad m => [Text] -> Source m Text
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
              _       -> fail ""

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
main = usingLoggerName mempty $ do
    initLogging Info
    Args{..} <- liftIO getOptions
    measures <- flip execStateT M.empty $
        forM_ inputFiles analyze
    printMeasures resultFile measures
