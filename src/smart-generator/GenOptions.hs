module GenOptions
       ( GenOptions (..)
       , optionsParser
       , optsInfo
       ) where

import           Options.Applicative (Parser, ParserInfo, auto, fullDesc, help, helper,
                                      info, long, many, metavar, option, progDesc, short,
                                      value)
import           Universum


import qualified Pos.CLI             as CLI

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
    , goJLFile          :: !(Maybe FilePath)
    , goCommonArgs      :: !CLI.CommonArgs -- ^ Common CLI arguments, including initial DHT nodes
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
    <*> CLI.optionalJSONPath
    <*> CLI.commonArgsParser "Initial DHT peer (may be many)"

optsInfo :: ParserInfo GenOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend` progDesc "Smart transaction generator"
