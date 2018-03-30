module JSONLog
    ( jsonLogs
    , parseLogP
    , IndexedJLTimedEvent (..)
    , runParseLogs
    ) where

import           Data.Attoparsec.Text (Parser, parseOnly, takeTill)
import           Pipes
import           Pipes.ByteString (fromHandle)
import           Pipes.Interleave (interleave)
import qualified Pipes.Prelude as P
import           System.Directory (listDirectory)
import           System.FilePath ((</>))

import           Pos.Util.JsonLog.Events (JLEvent, JLTimedEvent (..))
import           Types
import           Universum
import           Util.Aeson (parseJSONP)
import           Util.Safe (runWithFiles)

jsonLogs :: FilePath -> IO [(Text, FilePath)]
jsonLogs logDir = do
    files <- listDirectory logDir
    return $ map (second (logDir </>)) $ mapMaybe f files
  where
    f :: FilePath -> Maybe (Text, FilePath)
    f logFile = case parseOnly nodeIndexParser $ toText logFile of
        Right name -> Just (name, logFile)
        Left _  -> Nothing

nodeIndexParser :: Parser Text
nodeIndexParser = takeTill (== '.') <* ".json"

parseLogP :: MonadIO m => Handle -> Producer JLTimedEvent m ()
parseLogP h = fromHandle h >-> parseJSONP

data IndexedJLTimedEvent = IndexedJLTimedEvent
    { ijlNode      :: !NodeId
    , ijlTimestamp :: !Timestamp
    , ijlEvent     :: !JLEvent
    }

instance Eq IndexedJLTimedEvent where

    (==) = (==) `on` ijlTimestamp

instance Ord IndexedJLTimedEvent where

    compare = compare `on` ijlTimestamp

runParseLogs :: FilePath -> (Producer IndexedJLTimedEvent IO () -> IO r) -> IO r
runParseLogs logDir f = do
    xs <- jsonLogs logDir
    runWithFiles xs ReadMode $ \ys -> f $ interleave (map (uncurry producer) ys)
  where
    producer :: NodeId -> Handle -> Producer IndexedJLTimedEvent IO ()
    producer n h = parseLogP h >-> P.map (\JLTimedEvent{..} ->
        IndexedJLTimedEvent { ijlNode      = n
                            , ijlTimestamp = fromIntegral jlTimestamp
                            , ijlEvent     = jlEvent
                            })
