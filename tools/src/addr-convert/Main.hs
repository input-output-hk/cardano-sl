{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Universum

import           Data.Version (showVersion)
import           NeatInterpolation (text)
import           Options.Applicative (Parser, execParser, footerDoc, fullDesc, header, help, helper,
                                      info, infoOption, long, metavar, option, optional, progDesc,
                                      short)
import           Options.Applicative.Types (readerAsk)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Paths_cardano_sl (version)
import           Pos.Core (makeRedeemAddress)
import           Pos.Crypto.Signing (fromAvvmPk)
import           Pos.Util.Util (eitherToThrow)

data AddrConvertOptions = AddrConvertOptions
    { address :: !(Maybe Text)
    }

optionsParser :: Parser AddrConvertOptions
optionsParser = do
    address <- optional $ textOption $
           short   'a'
        <> long    "address"
        <> help    "Address to convert. It must be in base64(url) format."
        <> metavar "STRING"
    return AddrConvertOptions{..}
    where
      textOption = option (toText <$> readerAsk)

getAddrConvertOptions :: IO AddrConvertOptions
getAddrConvertOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc  "Produce public key and write it in stdout."
                 <> header    "Tool to convert vending addresses into mainnet/testnet addresses."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-addr-convert-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [text|
Command example:

  stack exec -- cardano-addr-convert -a 2HF83bvYCTzoCbVta6t64W8rFEnvnkJbIUFoT5tOyoU=

Output example:

  3mhNKjfhaCT13DjcQ9eMK4VHfZrFxmyXq8SjVPRtz7SWfP

You can also run it without arguments to switch to interactive mode.
In this case each entered vending address is echoed with a testnet address.|]

convertAddr :: Text -> IO Text
convertAddr addr =
    pretty . makeRedeemAddress <$>
    (eitherToThrow . fromAvvmPk) (toText addr)

main :: IO ()
main = do
    AddrConvertOptions{..} <- getAddrConvertOptions
    case address of
        Just addr -> convertAddr addr >>= putText
        Nothing   -> forever (getLine >>= convertAddr >>= putText)
