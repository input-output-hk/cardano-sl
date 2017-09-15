-- | Module for command-line utilites, parsers and convenient handlers.

module Pos.Client.CLI.Util
       ( printFlags
       , attackTypeParser
       , attackTargetParser
       , defaultLoggerConfig
       , getNodeSystemStart
       , readLoggerConfig
       , sscAlgoParser
       , stakeholderIdParser
       , dumpGenesisData
       ) where

import           Universum

import           Control.Exception.Safe (throwString)
import           Control.Lens           (zoom, (?=))
import qualified Crypto.Hash            as Hash
import qualified Data.ByteString.Lazy   as BSL
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Data.Time.Units        (toMicroseconds)
import           Formatting             (sformat, shown, (%))
import           Serokell.Util          (sec)
import           System.Wlog            (LoggerConfig (..), Severity (Info, Warning),
                                         fromScratch, lcTree, ltSeverity,
                                         parseLoggerConfig, zoomLogger)
import           Text.JSON.Canonical    (renderCanonicalJSON, toJSON)
import           Text.Parsec            (try)
import qualified Text.Parsec.Char       as P
import qualified Text.Parsec.Text       as P

import           Pos.Binary.Core        ()
import           Pos.Constants          (isDevelopment)
import           Pos.Core               (StakeholderId, Timestamp (..))
import           Pos.Core.Genesis       (mkGenesisData, staticSystemStart)
import           Pos.Crypto             (decodeAbstractHash)
import           Pos.Security.Params    (AttackTarget (..), AttackType (..))
import           Pos.Ssc.SscAlgo        (SscAlgo (..))
import           Pos.Util               (eitherToFail, inAssertMode)
import           Pos.Util.TimeWarp      (addrParser)

printFlags :: IO ()
printFlags = do
    if isDevelopment
        then putText "[Attention] We are in DEV mode"
        else putText "[Attention] We are in PRODUCTION mode"
    inAssertMode $ putText "Asserts are ON"

-- | Decides which secret-sharing algorithm to use.
sscAlgoParser :: P.Parser SscAlgo
sscAlgoParser = GodTossingAlgo <$ (P.string "GodTossing") <|>
                NistBeaconAlgo   <$ (P.string "NistBeacon")

attackTypeParser :: P.Parser AttackType
attackTypeParser = P.string "No" >>
    AttackNoBlocks <$ (P.string "Blocks") <|>
    AttackNoCommitments <$ (P.string "Commitments")

stakeholderIdParser :: P.Parser StakeholderId
stakeholderIdParser = do
    token <- some P.alphaNum
    eitherToFail $ decodeAbstractHash (toText token)

attackTargetParser :: P.Parser AttackTarget
attackTargetParser =
    (PubKeyAddressTarget <$> try stakeholderIdParser) <|>
    (NetworkAddressTarget <$> addrParser)

-- | Default logger config. Will be used if `--log-config` argument is
-- not passed. Corresponds to next logger config:
--
-- > node:
-- >   severity: Info
-- >   comm:
-- >     severity: Warning
--
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = fromScratch $ zoom lcTree $ zoomLogger "node" $ do
    ltSeverity ?= Info
    zoomLogger "comm" $ ltSeverity ?= Warning

-- | Reads logger config from given path. By default return
-- 'defaultLoggerConfig'.
readLoggerConfig :: MonadIO m => Maybe FilePath -> m LoggerConfig
readLoggerConfig = maybe (return defaultLoggerConfig) parseLoggerConfig

-- | This function carries out special logic to convert given
-- timestamp to the system start time.
getNodeSystemStart :: MonadIO m => Maybe Timestamp -> m Timestamp
getNodeSystemStart Nothing =
    case staticSystemStart of
        Nothing ->
            liftIO $
            throwString
                "Can't get system start, it's not known from GenesisSpec and wasn't passed via CLI"
        Just timestamp -> pure timestamp
getNodeSystemStart (Just cliOrConfigSystemStart)
  | isJust staticSystemStart =
      liftIO $ throwString
          "System start was passed via CLI, but it's also stored in 'GenesisSpec', can't choose"
  | cliOrConfigSystemStart >= 1400000000 =
    -- UNIX time 1400000000 is Tue, 13 May 2014 16:53:20 GMT.
    -- It was chosen arbitrarily as some date far enough in the past.
    -- See CSL-983 for more information.
    pure cliOrConfigSystemStart
  | otherwise = do
    let frameLength = timestampToSeconds cliOrConfigSystemStart
    currentPOSIXTime <- liftIO $ round <$> getPOSIXTime
    -- The whole timeline is split into frames, with the first frame starting
    -- at UNIX epoch start. We're looking for a time `t` which would be in the
    -- middle of the same frame as the current UNIX time.
    let currentFrame = currentPOSIXTime `div` frameLength
        t = currentFrame * frameLength + (frameLength `div` 2)
    pure $ Timestamp $ sec $ fromIntegral t
  where
    timestampToSeconds :: Timestamp -> Integer
    timestampToSeconds = (`div` 1000000) . toMicroseconds . getTimestamp

-- | Dump our 'GenesisData' into a file.
dumpGenesisData :: MonadIO m => Timestamp -> FilePath -> m ()
dumpGenesisData systemStart path = do
    putText $ sformat ("Writing JSON with hash "%shown%" to "%shown) jsonHash path
    liftIO $ BSL.writeFile path canonicalJsonBytes
  where
    jsonHash :: Hash.Digest Hash.Blake2b_256
    jsonHash = Hash.hash $ BSL.toStrict canonicalJsonBytes

    canonicalJsonBytes = renderCanonicalJSON $ runIdentity $ toJSON genesisData
    genesisData = mkGenesisData systemStart
