{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Universum

import           Data.Text.Read (decimal)
import           Data.Version (showVersion)
import           NeatInterpolation (text)
import           Options.Applicative (Parser, execParser, footerDoc, fullDesc, header, help, helper,
                                      info, infoOption, long, metavar, option, optional, progDesc,
                                      short)
import           Options.Applicative.Types (readerAsk)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Paths_cardano_sl (version)
import           Pos.Core (makeRedeemAddress)
import           Pos.Core.NetworkMagic (NetworkMagic (..), makeNetworkMagic)
import           Pos.Crypto.Configuration (ProtocolMagic (..), ProtocolMagicId (..),
                                           RequiresNetworkMagic (..))
import           Pos.Crypto.Signing (fromAvvmPk)
import           Pos.Util.Util (eitherToThrow)

data AddrConvertOptions = AddrConvertOptions
    { address       :: !(Maybe Text)
    , protocolMagic :: !Text
    }

optionsParser :: Parser AddrConvertOptions
optionsParser = do
    address <- optional $ textOption $
           short   'a'
        <> long    "address"
        <> help    "Address to convert. It must be in base64(url) format."
        <> metavar "STRING"
    pm      <- textOption $
           short   'p'
        <> long    "protocolMagic"
        <> help    "Generate mainnet or testnet address."
        <> metavar "STRING"
    return $ AddrConvertOptions address pm
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

  stack exec -- cardano-addr-convert -a 2HF83bvYCTzoCbVta6t64W8rFEnvnkJbIUFoT5tOyoU= -p mainnet

Output example:

  Ae2tdPwUPEZLTt73QktZUahV9FXnBRJNgzpbueBqQKGyPDgPdzPdwVretyk

You can also run it without the address argument to switch to interactive mode.
In this case each entered vending address is echoed with a testnet address.|]

convertAddr :: NetworkMagic -> Text -> IO Text
convertAddr nm addr =
    pretty . (makeRedeemAddress nm) <$>
    (eitherToThrow . fromAvvmPk) (toText addr)

-- Both mainnet & staging use NMNothing. We explicity disallow provision of
-- their integer values, so as to avoid confusion with NMJust.
toNetworkMagic :: Text -> Either Text NetworkMagic
toNetworkMagic txt =
    case txt of
        "mainnet" -> Right NMNothing
        "staging" -> Right NMNothing

        num -> case decimal num of
            Right (pm, _)
                | pm == stagingProtocolMagic ->
                    Left "Got staging's ProtocolMagic;\
                        \ please enter 'staging' instead"
                | pm == mainnetProtocolMagic ->
                    Left "Got mainnet's ProtocolMagic;\
                        \ please enter 'mainnet' instead"
            Right (ident, _) ->
                Right (makeNetworkMagic (ProtocolMagic (ProtocolMagicId ident)
                                        NMMustBeJust))
            Left err -> Left $ toText ("Please enter either 'mainnet', 'staging', or\
                                      \ an Int32 value: " ++ err)

-- As documented [here](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-844), these are
-- the ProtocolMagic Int32 values of mainnet & staging.
stagingProtocolMagic :: Int32
stagingProtocolMagic = 633343913

mainnetProtocolMagic :: Int32
mainnetProtocolMagic = 764824073

main :: IO ()
main = do
    AddrConvertOptions{..} <- getAddrConvertOptions
    case (address, toNetworkMagic protocolMagic) of
        (_        , Left txt) -> putTextLn txt >> exitFailure
        (Just addr, Right nm) -> convertAddr nm addr >>= putTextLn
        (Nothing  , Right nm) -> forever (getLine >>= convertAddr nm >>= putTextLn)
