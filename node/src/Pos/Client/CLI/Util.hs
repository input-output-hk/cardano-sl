-- | Module for command-line utilites, parsers and convenient handlers.

module Pos.Client.CLI.Util
       ( printFlags
       , attackTypeParser
       , attackTargetParser
       , defaultLoggerConfig
       , readLoggerConfig
       , sscAlgoParser
       , stakeholderIdParser
       , dumpGenesisData
       ) where

import           Universum

import           Control.Lens           (zoom, (?=))
import qualified Data.ByteString.Lazy   as BSL
import           Formatting             (sformat, shown, (%))
import           System.Wlog            (LoggerConfig (..), Severity (Info, Warning),
                                         fromScratch, lcTree, ltSeverity,
                                         parseLoggerConfig, zoomLogger)
import           Text.Parsec            (try)
import qualified Text.Parsec.Char       as P
import qualified Text.Parsec.Text       as P

import           Pos.Binary.Core        ()
import           Pos.Constants          (isDevelopment)
import           Pos.Core               (StakeholderId)
import           Pos.Core.Configuration (HasConfiguration, canonicalGenesisJson,
                                         genesisData)
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

-- | Dump our 'GenesisData' into a file.
--
-- FIXME avieth
-- system start parameter isn't needed. The genesis data can be derived from
-- the HasConfiguration constraint.
dumpGenesisData :: (HasConfiguration, MonadIO m) => FilePath -> m ()
dumpGenesisData path = do
    let (canonicalJsonBytes, jsonHash) = canonicalGenesisJson genesisData
    putText $ sformat ("Writing JSON with hash "%shown%" to "%shown) jsonHash path
    liftIO $ BSL.writeFile path canonicalJsonBytes
