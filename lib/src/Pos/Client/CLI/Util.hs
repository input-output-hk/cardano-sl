-- | Module for command-line utilites, parsers and convenient handlers.

module Pos.Client.CLI.Util
       ( printFlags
       , printInfoOnStart
       , attackTypeParser
       , attackTargetParser
       , defaultLoggerConfig
       , readLoggerConfig
       , stakeholderIdParser
       , dumpGenesisData
       , dumpConfiguration
       ) where

import           Universum hiding (try)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Yaml as Yaml
import           Formatting (sformat, shown, (%))
import           System.Wlog (LoggerConfig (..), WithLogger, logInfo,
                     parseLoggerConfig, productionB)
import           Text.Parsec (parserFail, try)
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Text as P

import           Ntp.Client (NtpConfiguration)
import           Pos.Chain.Block (blockConfiguration)
import           Pos.Chain.Delegation (dlgConfiguration)
import           Pos.Chain.Security (AttackTarget (..), AttackType (..))
import           Pos.Chain.Ssc (sscConfiguration)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Chain.Update (updateConfiguration)
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..))
import           Pos.Client.CLI.Options (configurationOptions)
import           Pos.Configuration (nodeConfiguration)
import           Pos.Core (StakeholderId, Timestamp (..))
import           Pos.Core.Conc (currentTime)
import           Pos.Core.Configuration (HasConfiguration, canonicalGenesisJson,
                     coreConfiguration, genesisData, prettyGenesisJson)
import           Pos.Core.Genesis (gdStartTime)
import           Pos.Core.NetworkAddress (addrParser)
import           Pos.Crypto (decodeAbstractHash)
import           Pos.Launcher.Configuration (Configuration (..),
                     HasConfigurations)
import           Pos.Util.AssertMode (inAssertMode)

printFlags :: WithLogger m => m ()
printFlags = do
    inAssertMode $ logInfo "Asserts are ON"

printInfoOnStart ::
       (HasConfigurations, WithLogger m, MonadIO m)
    => CommonNodeArgs
    -> NtpConfiguration
    -> TxpConfiguration
    -> m ()
printInfoOnStart CommonNodeArgs {..} ntpConfig txpConfig = do
    whenJust cnaDumpGenesisDataPath $ dumpGenesisData True
    when cnaDumpConfiguration $ dumpConfiguration ntpConfig txpConfig
    printFlags
    t <- currentTime
    mapM_ logInfo $
        [ sformat ("System start time is " % shown) $ gdStartTime genesisData
        , sformat ("Current time is "%shown) (Timestamp t)
        , sformat ("Using configs and genesis:\n"%shown)
                  (configurationOptions commonArgs)
        ]

attackTypeParser :: P.Parser AttackType
attackTypeParser = P.string "No" >>
    AttackNoBlocks <$ (P.string "Blocks") <|>
    AttackNoCommitments <$ (P.string "Commitments")

stakeholderIdParser :: P.Parser StakeholderId
stakeholderIdParser = do
    token <- some P.alphaNum
    either (parserFail . toString) return $
        decodeAbstractHash (toText token)

attackTargetParser :: P.Parser AttackTarget
attackTargetParser =
    (PubKeyAddressTarget <$> try stakeholderIdParser) <|>
    (NetworkAddressTarget <$> addrParser)

-- | Default logger config. Will be used if `--log-config` argument is
-- not passed.
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = productionB

-- | Reads logger config from given path. By default returns
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

-- | Dump our configuration into stdout and exit.
dumpConfiguration
    :: (HasConfigurations, MonadIO m)
    => NtpConfiguration
    -> TxpConfiguration
    -> m ()
dumpConfiguration ntpConfig txpConfig = do
    let conf =
            Configuration
            { ccCore = coreConfiguration
            , ccNtp = ntpConfig
            , ccUpdate = updateConfiguration
            , ccSsc = sscConfiguration
            , ccDlg = dlgConfiguration
            , ccTxp = txpConfig
            , ccBlock = blockConfiguration
            , ccNode = nodeConfiguration
            }
    putText . decodeUtf8 . Yaml.encode $ conf
    exitSuccess
