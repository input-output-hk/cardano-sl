module GenOptions
       ( GenOptions (..)
       , optionsParser
       , optsInfo
       ) where

import           Options.Applicative    (Parser, ParserInfo, auto, fullDesc, help, helper,
                                         info, long, many, metavar, option, progDesc,
                                         short, showDefault, switch, value)
import           Serokell.Util.OptParse (fromParsec, strOption)
import           Universum


import           Pos.CLI                (dhtNodeParser, sscAlgoParser)
import           Pos.DHT                (DHTNode)
import           Pos.Ssc.SscAlgo        (SscAlgo (..))

data GenOptions = GenOptions
    { goGenesisIdxs        :: ![Word]       -- ^ Index in genesis key pairs.
    -- , goRemoteAddr  :: !NetworkAddress -- ^ Remote node address
    , goDHTPeers           :: ![DHTNode]  -- ^ Initial DHT nodes
    , goRoundPeriodRate    :: !Int        -- ^ R, where duration of one round is ((k + P) * (R + 1)) * slotDuration
    , goRoundNumber        :: !Int        -- ^ Number of rounds
    , goRoundPause         :: !Double     -- ^ Pause between rounds (in seconds)
    , goInitBalance        :: !Int        -- ^ Total coins in init utxo per address
    , goInitTps            :: !Double     -- ^ Start TPS rate (it adjusts over time)
    , goTpsIncreaseStep    :: !Double     -- ^ When system is stable, increase TPS in next round by this value
    , goPropThreshold      :: !Int
    , goRecipientShare     :: !Double     -- ^ Which portion of neighbours to send on each round
    , goDhtExplicitInitial :: !Bool
    , goLogConfig          :: !(Maybe FilePath)
    , goLogsPrefix         :: !(Maybe FilePath)
    , goJLFile             :: !(Maybe FilePath)
    , goSscAlgo            :: !SscAlgo
    , goFlatDistr          :: !(Maybe (Int, Int))
    , goBitcoinDistr       :: !(Maybe (Int, Int))
    , goDisablePropagation :: !Bool
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
    <*> many (option (fromParsec dhtNodeParser) $
             long "peer"
          <> metavar "HOST:PORT/HOST_ID"
          <> help "Initial DHT peer (may be many)")
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
    <*> switch
        (long "explicit-initial" <>
         help
             "Explicitely contact to initial peers as to neighbors (even if they appeared offline once)")
    <*> optional (strOption $
                  long "log-config"
               <> metavar "FILEPATH"
               <> help "Path to logger configuration")
    <*> optional (strOption $
                  long "logs-prefix"
               <> metavar "FILEPATH"
               <> help "Prefix to logger output path")
    <*> optional (strOption $
                  long "json-log"
               <> metavar "FILEPATH"
               <> help "Path to json log file")
    <*> option (fromParsec sscAlgoParser)
        (long "ssc-algo"
      <> metavar "ALGO"
      <> value GodTossingAlgo
      <> showDefault
      <> help "Shared Seed Calculation algorithm which nodes will use")
    <*> optional
        (option auto $
         mconcat
            [ long "flat-distr"
            , metavar "(INT,INT)"
            , help "Use flat stake distribution with given parameters (nodes, coins)"
            ])
    <*> optional
        (option auto $
         mconcat
            [ long "bitcoin-distr"
            , metavar "(INT,INT)"
            , help "Use bitcoin stake distribution with given parameters (nodes, coins)"
            ])
    <*> switch
        (long "disable-propagation" <>
         help "Disable network propagation (transactions, SSC data, blocks). I.e. all data is to be sent only by entity who creates data and entity is yosend it to all peers on his own")

optsInfo :: ParserInfo GenOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend` progDesc "Smart transaction generator"
