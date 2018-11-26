{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Universum

import           Data.Aeson (ToJSON, genericToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Prelude
import           System.Console.Docopt (Arguments, Docopt, argument, command,
                     docopt, exitWithUsage, getArgOrExitWith, isPresent,
                     longOption, parseArgsOrExit)
import qualified System.Console.Docopt as Docopt

import           Pos.Binary.Class (Bi, decodeFull')
import           Pos.Chain.Txp (TxAux (..), TxInWitness (..), TxSigData (..))
import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..), SignTag (SignTx), checkSig,
                     hash)


-- | Command-Line Interface specification. See http://docopt.org/
cli :: Docopt
cli = [docopt|
cardano-sl-tx-utils

Yet another set of extra (lightweight) tools for cardano-sl.

Usage:
  cardano-sl-tx-utils decode [options] <bytes>
  cardano-sl-tx-utils verify signatures [options] <bytes>

Options:
  -h, --help            Show this help
  --requires-magic      Requires network magic (e.g. testnet)
  --protocol-magic=INT  Protocol magic id [default: 764824073]
|]

main :: IO ()
main = do
    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $
        exitWithUsage cli

    bytes <- args `getArgOrExit` (argument "bytes")
    pm <- getProtocolMagicOrExit args

    when (args `isPresent` (command "decode")) $
        txDecode bytes

    when (args `isPresent` (command "verify") && args `isPresent` (command "signatures")) $
        txVerifySignatures pm bytes


--
-- COMMANDS
--

-- | Decode a 'TxAux' from base16 and pretty-print it
txDecode :: String -> IO ()
txDecode =
    decodeBase16 @TxAux >=> prettyPrint

-- | Decode a 'TxAux' from base16 and pretty-print signatures validations
txVerifySignatures :: ProtocolMagic -> String -> IO ()
txVerifySignatures pm hex = do
    txAux <- decodeBase16 @TxAux hex
    forM_ (taWitness txAux) $ \(PkWitness key sig) -> do
        let sigData = TxSigData $ hash $ taTx txAux
        prettyPrint (checkSigPretty SignTx key sigData sig, key)
  where
    checkSigPretty tag key sigData sig =
        if checkSig pm tag key sigData sig then
            ValidSignature
        else
            InvalidSignature


--
-- Internal
--

data SignatureValidation
    = ValidSignature
    | InvalidSignature
    deriving (Show, Generic)

instance ToJSON SignatureValidation where
    toJSON = genericToJSON Aeson.defaultOptions


decodeBase16 :: Bi a => String -> IO a
decodeBase16 =
    either (fail . show) return . decodeFull' . fst . Base16.decode . B8.pack

prettyPrint :: ToJSON a => a -> IO ()
prettyPrint =
    B8.putStrLn . BL.toStrict . Aeson.encodePretty

getArgOrExit :: Arguments -> Docopt.Option -> IO String
getArgOrExit = getArgOrExitWith cli

getProtocolMagicOrExit :: Arguments -> IO ProtocolMagic
getProtocolMagicOrExit args = ProtocolMagic
    <$> protocolMagicId
    <*> pure requiresNetworkMagic
  where
    protocolMagicId :: IO ProtocolMagicId
    protocolMagicId =
        fmap (ProtocolMagicId . Prelude.read) $
            args `getArgOrExit` (longOption "protocol-magic")

    requiresNetworkMagic :: RequiresNetworkMagic
    requiresNetworkMagic =
        if args `isPresent` (longOption "requires-magic") then
            RequiresMagic
        else
            RequiresNoMagic
