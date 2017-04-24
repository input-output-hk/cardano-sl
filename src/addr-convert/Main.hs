-- | A tool to convert vending addresses into testnet addresses.

module Main (main) where

import           Universum

import qualified Data.ByteString      as BS
import qualified Data.Text            as T
import qualified Serokell.Util.Base64 as B64

import           Pos.Crypto           (RedeemPublicKey (..), redeemPkBuild)
import           Pos.Types            (makeRedeemAddress)

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

usage :: Text
usage =
    "Usage:\n\
    \\n\
    \    cardano-addr-convert <address>\n\
    \\n\
    \You can also run it without arguments to switch to interactive mode,\n\
    \in which each entered vending address is echoed with a testnet address\n"

convertAddr :: Text -> IO Text
convertAddr addr = pretty . makeRedeemAddress <$> fromAvvmPk (toText addr)

-- Sample output:
--
-- > cardano-addr-convert 2HF83bvYCTzoCbVta6t64W8rFEnvnkJbIUFoT5tOyoU=
-- 3mhNKjfhaCT13DjcQ9eMK4VHfZrFxmyXq8SjVPRtz7SWfP
main :: IO ()
main = do
    args <- map toText <$> getArgs
    case args of
        []         -> forever (getLine >>= convertAddr >>= putText)
        ["-h"]     -> putText usage
        ["--help"] -> putText usage
        [addr]     -> convertAddr addr >>= putText
        _          -> putText usage
