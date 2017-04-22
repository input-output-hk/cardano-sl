{-# LANGUAGE ApplicativeDo #-}

module GenOptions
       ( GenOptions (..)
       , optionsParser
       , optsInfo
       ) where

import           Data.Version        (showVersion)
import           Options.Applicative (Parser, ParserInfo, auto, fullDesc, help, helper,
                                      info, long, many, metavar, option, optional,
                                      progDesc, short, value)
import           Universum

import           Paths_cardano_sl    (version)

import qualified Pos.CLI             as CLI
import           Pos.Communication   (NodeId)

data GenOptions = GenOptions
    { goGenesisIdxs     :: ![Word]       -- ^ Index in genesis key pairs.
    -- , goRemoteAddr  :: !NetworkAddress -- ^ Remote node address
    , goRoundPeriodRate :: !Int        -- ^ R, where duration of one round is ((k + P) * (R + 1)) * slotDuration
    , goRoundNumber     :: !Int        -- ^ Number of rounds
    , goRoundPause      :: !Double     -- ^ Pause between rounds (in seconds)
    , goInitBalance     :: !Int        -- ^ Total coins in init utxo per address
    , goInitTps         :: !Double     -- ^ Start TPS rate (it adjusts over time)
    , goTpsIncreaseStep :: !Double     -- ^ When system is stable, increase TPS in next round by this value
    , goPropThreshold   :: !Int
    , goRecipientShare  :: !Double     -- ^ Which portion of neighbours to send on each round
    , goMOfNParams      :: !(Maybe (Int, Int)) -- ^ If this is provided, send M-of-N script transactions instead of P2PKH
    , goJLFile          :: !(Maybe FilePath)
    , goPeers           :: ![NodeId]
      -- ^ The peers to contact.
    , goCommonArgs      :: !CLI.CommonArgs -- ^ Common CLI arguments.
    }

optionsParser :: Parser GenOptions
optionsParser = do
    goGenesisIdxs <- many $ option auto $
        short   'i' <>
        long    "index" <>
        metavar "INT" <>
        help    "Index in list of genesis key pairs"
    goRoundPeriodRate <- option auto $
        short 'R' <>
        long  "round-period-rate" <>
        value 2 <>
        help  "R, where duration of one round is \
              \((k + P) * (R + 1)) * slotDuration"
    goRoundNumber <- option auto $
        short 'N' <>
        long "round-number" <>
        help "Number of testing rounds"
    goRoundPause <- option auto $
        short 'p' <>
        long  "round-pause" <>
        value 0 <>
        help  "Pause between rounds (in seconds)"
    goInitBalance <- option auto $
        long "init-money" <>
        help "How many coins node has in the beginning"
    goInitTps <- option auto $
        short   't' <>
        long    "tps" <>
        metavar "DOUBLE" <>
        help    "TPS (transactions per second)"
    goTpsIncreaseStep <- option auto $
        short   'S' <>
        long    "tps-step" <>
        value   10 <>
        metavar "DOUBLE" <>
        help    "TPS increase delta on stable system"
    goPropThreshold <- option auto $
        short 'P' <>
        long  "propagate-threshold" <>
        value 1 <>
        help  "Approximate number of slots needed to propagate \
              \transactions across the network"
    goRecipientShare <- option auto $
        long  "recipients-share" <>
        value 1 <>
        help  "Which portion of neighbours to send on each round"
    goMOfNParams <- optional $ option auto $
        long "m-of-n" <>
        metavar "(M, N)" <>
        help "If enabled, send M-of-N txs instead of regular ones"
    goJLFile <-
        CLI.optionalJSONPath
    goPeers <- many $ CLI.nodeIdOption "peer" "Address of a peer (host:port:peer_id)"
    goCommonArgs <-
        CLI.commonArgsParser
    return GenOptions{..}

optsInfo :: ParserInfo GenOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend`
    progDesc ("Smart transaction generator version " <> showVersion version)
