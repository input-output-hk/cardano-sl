module Pos.DHT.Real.CLI
       ( dhtExplicitInitialOption
       , dhtNetworkAddressOption
       , dhtKeyOption
       , dhtNodeOption
       , dhtPeersFileOption
       , readDhtPeersFile
       ) where

import           Universum

import           Control.Exception.Safe (throwString)
import           Formatting (build, formatToString, shown, (%))
import qualified Options.Applicative as Opt
import           Pos.Util.OptParse (fromParsec)
import           Text.Parsec (eof, parse)

import           Pos.DHT.Model.Types (DHTKey, DHTNode, dhtKeyParser, dhtNodeParser)
import           Pos.Util.TimeWarp (NetworkAddress, addrParser)

dhtExplicitInitialOption :: Opt.Parser Bool
dhtExplicitInitialOption =
    Opt.switch
        (Opt.long "kademlia-explicit-initial" <>
         Opt.help "Explicitely contact to initial peers as to neighbors (even if they\
                  \ appeared offline once)")

dhtNetworkAddressOption :: Maybe NetworkAddress -> Opt.Parser NetworkAddress
dhtNetworkAddressOption na =
    Opt.option (fromParsec addrParser) $
            Opt.long "kademlia-address"
         <> Opt.metavar "IP:PORT"
         <> Opt.help helpMsg
         <> Opt.showDefault
         <> maybe mempty Opt.value na
  where
    helpMsg = "Ip and port to which kademlia should bind."

dhtKeyOption :: Opt.Parser DHTKey
dhtKeyOption =
    Opt.option (fromParsec dhtKeyParser) $
        Opt.long "kademlia-id" <>
        Opt.metavar "HOST_ID" <>
        Opt.help "Kademlia id for this node in base64-url"

dhtNodeOption :: Opt.Parser DHTNode
dhtNodeOption =
    Opt.option (fromParsec dhtNodeParser) $
        Opt.long "kademlia-peer" <>
        Opt.metavar "HOST:PORT/HOST_ID" <>
        Opt.help "Identifier of a node in a Kademlia network"

dhtPeersFileOption :: Opt.Parser FilePath
dhtPeersFileOption =
    Opt.strOption $
        Opt.long "kademlia-peers-file" <>
        Opt.metavar "FILEPATH" <>
        Opt.help "Path to a file containing a newline-separated list of Kademlia peers"

readDhtPeersFile :: FilePath -> IO [DHTNode]
readDhtPeersFile path = do
    xs <- lines <$> readFile path
    let parseLine x = case parse (dhtNodeParser <* eof) "" x of
            Left err -> throwString $ formatToString
                ("error when parsing peer "%shown%
                 " from peers file "%build%": "%shown) x path err
            Right a -> return a
    mapM parseLine xs
