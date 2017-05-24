module Main (main) where

import           Universum

import           Data.String.QQ               (s)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Data.Version                 (showVersion)
import           Options.Applicative          (Parser, execParser, footerDoc, fullDesc,
                                               help, header, helper, info, infoOption, long,
                                               metavar, optional, progDesc, short)
import           Options.Applicative.Text     (textOption)
import qualified Data.ByteString              as BS
import qualified Data.Text                    as T
import qualified Serokell.Util.Base64         as B64

import           Paths_cardano_sl             (version)
import           Pos.Crypto                   (RedeemPublicKey (..), redeemPkBuild)
import           Pos.Types                    (makeRedeemAddress)

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

getAddrConvertOptions :: IO AddrConvertOptions
getAddrConvertOptions = execParser programInfo >>= return
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc  "Produce public key and write it in stdout."
                 <> header    "Tool to convert vending addresses into testnet addresses."
                 <> footerDoc usageExample
                 
    versionOption = infoOption
        ("cardano-addr-convert-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = Just [s|
Command example:

  stack exec -- cardano-addr-convert -a 2HF83bvYCTzoCbVta6t64W8rFEnvnkJbIUFoT5tOyoU=

Output example:

  3mhNKjfhaCT13DjcQ9eMK4VHfZrFxmyXq8SjVPRtz7SWfP

You can also run it without arguments to switch to interactive mode.
In this case each entered vending address is echoed with a testnet address.|]

main :: IO ()
main = do
    AddrConvertOptions{..} <- getAddrConvertOptions
    case address of
        Just addr -> convertAddr addr >>= putText
        Nothing   -> forever (getLine >>= convertAddr >>= putText)

-- | Read the text into a redeeming public key.
--
-- Copied from keygen/Avvm.hs.
fromAvvmPk :: (MonadFail m, Monad m) => Text -> m RedeemPublicKey
fromAvvmPk addrText = do
    let base64rify = T.replace "-" "+" . T.replace "_" "/"
    let parsedM = B64.decode $ base64rify addrText
    addrParsed <-
        maybe (fail $ "Address " <> toString addrText <> " is not base64(url) format")
        pure
        (rightToMaybe parsedM)
    unless (BS.length addrParsed == 32) $
        fail "Address' length is not equal to 32, can't be redeeming pk"
    pure $ redeemPkBuild addrParsed

convertAddr :: Text -> IO Text
convertAddr addr = pretty . makeRedeemAddress <$> fromAvvmPk (toText addr)
