{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

module GenOptions
       ( GenOptions (..)
       , getGenOptions
       ) where

import           Data.String.QQ               (s)
import           Data.Version                 (showVersion)
import           Options.Applicative          (Parser, auto, execParser, footerDoc,
                                               fullDesc, header, help, helper, info,
                                               infoOption, long, metavar, option,
                                               progDesc, short, value)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Universum

import           Paths_cardano_sl             (version)

import qualified Pos.CLI                      as CLI
import           Pos.Communication            (NodeId)

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
        help    "Index in list of genesis key pairs. This option can be defined more than once."
    goRoundPeriodRate <- option auto $
        short 'R' <>
        long  "round-period-rate" <>
        value 2 <>
        help  "R, where duration of one round is \
              \((k + P) * (R + 1)) * slotDuration."
    goRoundNumber <- option auto $
        short 'N' <>
        long "round-number" <>
        help "Number of testing rounds."
    goRoundPause <- option auto $
        short 'p' <>
        long  "round-pause" <>
        value 0 <>
        help  "Pause between rounds, in seconds."
    goInitBalance <- option auto $
        long "init-money" <>
        help "How many coins node has in the beginning."
    goInitTps <- option auto $
        short   't' <>
        long    "tps" <>
        metavar "DOUBLE" <>
        help    "TPS (transactions per second)."
    goTpsIncreaseStep <- option auto $
        short   'S' <>
        long    "tps-step" <>
        value   10 <>
        metavar "DOUBLE" <>
        help    "TPS increase delta on stable system."
    goPropThreshold <- option auto $
        short 'P' <>
        long  "propagate-threshold" <>
        value 1 <>
        help  "Approximate number of slots needed to propagate \
              \transactions across the network."
    goRecipientShare <- option auto $
        long  "recipients-share" <>
        value 1 <>
        help  "Which portion of neighbours to send on each round."
    goMOfNParams <- optional $ option auto $
        long "m-of-n" <>
        metavar "(M, N)" <>
        help "If enabled, send M-of-N txs instead of regular ones."
    goJLFile <-
        CLI.optionalJSONPath
    goPeers <- many $ CLI.nodeIdOption "peer" "Address of a peer. This option can be defined more than once."
    goCommonArgs <-
        CLI.commonArgsParser
    return GenOptions{..}

getGenOptions :: IO GenOptions
getGenOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc "It starts a stress test to see an actual number of generated transactions per second (TPS)."
                 <> header "Cardano SL smart transaction generator."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-smart-generator-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = Just [s|
Command example:

  stack exec -- cardano-smart-generator     \
    -i 0                                    \
    --disable-propagation                   \
    --peer 35.157.97.210:3000               \
    -R 4                                    \
    -N 100                                  \
    -p 30                                   \
    --init-money 60000000                   \
    -t 1                                    \
    -S 1                                    \
    -P 2                                    \
    --recipients-share 0.3                  \
    --log-config static/txgen-logging.yaml  \
    --json-log txgen.json                   \
    --flat-distr "(80,60000000)"|]
