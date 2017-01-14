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
import           Pos.Util.TimeWarp   (NetworkAddress)

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
    , goCommonArgs      :: !CLI.CommonArgs -- ^ Common CLI arguments, including initial DHT nodes
    , goIpPort          :: !NetworkAddress         -- ^ DHT/Blockchain ip/port
    , goNtpPort         :: !(Maybe Word16)  -- ^ Ntp port, by default (main port + 1000) is used
    }

optionsParser :: Parser GenOptions
optionsParser = GenOptions
    <$> many (option auto
            (short 'i'
          <> long "index"
          <> metavar "INT"
          <> help "Index in list of genesis key pairs"))
    -- <*> option (fromParsec addrParser)
    --         (long "peer"
    --       <> metavar "HOST:PORT"
    --       <> help "Node address to ZERG RUSH")
    <*> option auto
            (short 'R'
          <> long "round-period-rate"
          <> value 2
          <> help "R, where duration of one round is ((k + P) * (R + 1)) * slotDuration")
    <*> option auto
            (short 'N'
          <> long "round-number"
          <> help "Number of testing rounds")
    <*> option auto
            (short 'p'
          <> long "round-pause"
          <> value 0
          <> help "Pause between rounds (in seconds)")
    <*> option auto
            (long "init-money"
          <> help "How many coins node has in the beginning")
    <*> option auto
            (short 't'
          <> long "tps"
          <> metavar "DOUBLE"
          <> help "TPS (transactions per second)")
    <*> option auto
            (short 'S'
          <> long "tps-step"
          <> value 10
          <> metavar "DOUBLE"
          <> help "TPS increase delta on stable system")
    <*> option auto
            (short 'P'
          <> long "propagate-threshold"
          <> value 1
          <> help "Approximate number of slots needed to propagate transactions across the network")
    <*> option auto
            (long "recipients-share"
          <> value 1
          <> help "Which portion of neighbours to send on each round")
    <*> optional
            (option auto $
             long "m-of-n"
          <> metavar "(M, N)"
          <> help "If enabled, send M-of-N txs instead of regular ones")
    <*> CLI.optionalJSONPath
    <*> CLI.commonArgsParser "Initial DHT peer (may be many)"
    <*> CLI.ipPortOption ("0.0.0.0", 24962)
    <*> optional
            (option auto $
             long "ntp-port"
          <> metavar "INT"
          <> help "Port for NTP client")

optsInfo :: ParserInfo GenOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend`
    progDesc ("Smart transaction generator version " <> showVersion version)
