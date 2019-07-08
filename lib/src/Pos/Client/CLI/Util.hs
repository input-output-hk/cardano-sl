{-# LANGUAGE RecordWildCards #-}

-- | Module for command-line utilites, parsers and convenient handlers.

module Pos.Client.CLI.Util
       ( attackTypeParser
       , attackTargetParser
       , defaultLoggerConfig
       , readLoggerConfig
       , stakeholderIdParser
       ) where

import           Universum

import           Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import           Pos.Chain.Security (AttackTarget (..), AttackType (..))
import           Pos.Core (StakeholderId)
import           Pos.Core.NetworkAddress (addrParser)
import           Pos.Crypto (decodeAbstractHash)
import           Pos.Util.Wlog (LoggerConfig (..), parseLoggerConfig,
                     productionB)

attackTypeParser :: Parsec () String AttackType
attackTypeParser = P.string "No" >>
    AttackNoBlocks <$ (P.string "Blocks") <|>
    AttackNoCommitments <$ (P.string "Commitments")

stakeholderIdParser :: Parsec () String StakeholderId
stakeholderIdParser = do
    token <- some P.alphaNumChar
    either (fail . toString) return $
        decodeAbstractHash (toText token)

attackTargetParser :: Parsec () String AttackTarget
attackTargetParser =
    (PubKeyAddressTarget <$> P.try stakeholderIdParser) <|>
    (NetworkAddressTarget <$> addrParser)

-- | Default logger config. Will be used if `--log-config` argument is
-- not passed.
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = productionB

-- | Reads logger config from given path. By default returns
-- 'defaultLoggerConfig'.
readLoggerConfig :: MonadIO m => Maybe FilePath -> m LoggerConfig
readLoggerConfig = maybe (return defaultLoggerConfig) parseLoggerConfig
