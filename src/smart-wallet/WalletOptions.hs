
module WalletOptions
       ( WalletOptions (..)
       , optsInfo
       ) where

import           Options.Applicative    (Parser, ParserInfo, auto, fullDesc, help, helper,
                                         info, long, many, metavar, option, option,
                                         progDesc, short, showDefault, switch, value)
import           Serokell.Util.OptParse (fromParsec, strOption)
import           Universum


import           Pos.CLI                (dhtNodeParser, sscAlgoParser)
import           Pos.DHT                (DHTNode)
import           Pos.Ssc.SscAlgo        (SscAlgo (..))


data WalletOptions = WalletOptions
    { woDHTPeers           :: ![DHTNode]  -- ^ Initial DHT nodes
    , woDhtExplicitInitial :: !Bool
    , woInitialPause       :: !Int     -- ^ Pause between connecting to network
                                       -- and starting accepting commands (in slots)
    , woSecretKeyIdx       :: !Int     -- ^ Index of genesis SK to use
    , woLogConfig          :: !(Maybe FilePath)
    , woLogsPrefix         :: !(Maybe FilePath)
    , woJLFile             :: !(Maybe FilePath)
    , woSscAlgo            :: !SscAlgo
    , woFlatDistr          :: !(Maybe (Int, Int))
    , woBitcoinDistr       :: !(Maybe (Int, Int))
    }

optionsParser :: Parser WalletOptions
optionsParser = WalletOptions
    <$> many (option (fromParsec dhtNodeParser) $
              long "peer"
           <> metavar "HOST:PORT/HOST_ID"
           <> help "Initial DHT peer (may be many)")
    <*> switch (long "explicit-initial"
             <> help "Explicitely contact to initial peers as to neighbors\
                     \ (even if they appeared offline once)")
    <*> option auto (long "initial-pause"
                  <> short 'p'
                  <> value 1
                  <> metavar "SLOTS_NUM"
                  <> help "Pause between connecting to network and starting\
                          \ accepting commands")
    <*> option auto (long "genesis-sk"
                  <> short 'i'
                  <> metavar "INDEX"
                  <> help "Index of genesis SK to use")
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

optsInfo :: ParserInfo WalletOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend` progDesc "Wallet-only node"

