module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Version (showVersion)
import           Network.Kademlia.HashNodeId (HashId (..), Nonce (..), genNonce, hashAddress)
import           Options.Applicative (Parser, execParser, fullDesc, header, help, helper, info,
                                      infoOption, long, metavar, progDesc, short, strOption)
import           Serokell.Util.Base64 (encodeUrl)
import           Universum

import           Paths_cardano_sl (version)
import           Pos.Crypto (runSecureRandom)

nonceLen :: Int
nonceLen = 14

generateKey :: BS.ByteString -> IO (Maybe BS.ByteString)
generateKey nonce =
  if length nonce /= nonceLen
  then if nonce == "-"
          then Just <$> randomDHTKey
          else pure Nothing
  else pure $ Just . unHashId . hashAddress $ Nonce nonce

unHashId :: HashId -> BS.ByteString
unHashId (HashId bs) = bs

randomDHTKey :: IO BS.ByteString
randomDHTKey = unHashId . hashAddress <$> runSecureRandom genNonce

data KeyGenOptions = KeyGenOptions
    { nonce :: !String
    } deriving (Show)

optionsParser :: Parser KeyGenOptions
optionsParser = do
    nonce <- strOption $
           short   'n'
        <> long    "nonce"
        <> metavar "STRING"
        <> help    "14-characters string."
    pure KeyGenOptions{..}

getKeyGenOptions :: IO KeyGenOptions
getKeyGenOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc ("Generated key will be print to stdout.")
                 <> header "Generator of random key for Kademlia DHT."

    versionOption = infoOption
        ("cardano-dht-keygen-" <> showVersion version)
        (long "version" <> help "Show version.")

main :: IO ()
main = do
    KeyGenOptions{..} <- getKeyGenOptions
    key <- generateKey $ BS.pack nonce
    putStrLn . fromMaybe "Invalid nonce length, it must be 14 characters." . fmap encodeUrl $ key
