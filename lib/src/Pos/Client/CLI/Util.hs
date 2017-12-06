-- | Module for command-line utilites, parsers and convenient handlers.

module Pos.Client.CLI.Util
       ( printFlags
       , attackTypeParser
       , attackTargetParser
       , defaultLoggerConfig
       , readLoggerConfig
       , stakeholderIdParser
       , dumpGenesisData
       ) where

import           Universum

import           Control.Lens (zoom, (?=))
import qualified Data.ByteString.Lazy as BSL
import           Formatting (sformat, shown, (%))
import           System.Wlog (LoggerConfig (..), Severity (Info, Warning), WithLogger, fromScratch,
                              lcTree, logInfo, ltSeverity, parseLoggerConfig, zoomLogger)
import           Text.Parsec (try)
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Text as P

import           Pos.Binary.Core ()
import           Pos.Core (StakeholderId)
import           Pos.Core.Configuration (HasConfiguration, canonicalGenesisJson, genesisData,
                                         prettyGenesisJson)
import           Pos.Crypto (decodeAbstractHash)
import           Pos.Security.Params (AttackTarget (..), AttackType (..))
import           Pos.Util (eitherToFail)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.TimeWarp (addrParser)

printFlags :: WithLogger m => m ()
printFlags = do
    inAssertMode $ logInfo "Asserts are ON"

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
dumpGenesisData ::
       (HasConfiguration, MonadIO m, WithLogger m) => Bool -> FilePath -> m ()
dumpGenesisData canonical path = do
    let (canonicalJsonBytes, jsonHash) = canonicalGenesisJson genesisData
    let prettyJsonStr = prettyGenesisJson genesisData
    logInfo $ sformat ("Writing JSON with hash "%shown%" to "%shown) jsonHash path
    liftIO $ case canonical of
        True  -> BSL.writeFile path canonicalJsonBytes
        False -> writeFile path (toText prettyJsonStr)
