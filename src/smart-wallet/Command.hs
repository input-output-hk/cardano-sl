{-# LANGUAGE ViewPatterns #-}

-- | Module for commands parsing

module Command
       ( Command (..)
       , parseCommand
       ) where

import           Control.Lens           (view, _3)
import           Control.Monad          (fail)
import           Data.Binary            (decodeOrFail)
import           Data.ByteString.Base58 (decodeBase58)
import qualified Data.ByteString.Lazy   as BSL (fromStrict)
import           Data.String            (String)
import           Prelude                (read, show)
import           Text.Parsec            (many1, parse, try, (<?>))
import           Text.Parsec.Char       (alphaNum, digit, spaces, string)
import           Text.Parsec.Text       (Parser)
import           Universum              hiding (show)

import           Pos.Types              (Address (..), Coin (..), TxOut (..),
                                         addrAlphabet)

data Command = Balance Address
             | Send [TxOut]
             | Quit

decodeAddress :: Text -> Either String Address
decodeAddress (encodeUtf8 -> bs) = do
    let base58Err = "Invalid base58 representation of address"
        takeErr = toString . view _3
        takeRes = view _3
    dbs <- maybeToRight base58Err $ decodeBase58 addrAlphabet bs
    bimap takeErr takeRes $ decodeOrFail $ BSL.fromStrict dbs

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p

text :: String -> Parser Text
text = lexeme . fmap toText . string

address :: Parser Address
address = lexeme $ toText <$> many1 alphaNum >>=
          either fail pure . decodeAddress

coin :: Parser Coin
coin = lexeme $ fromInteger . read <$> many1 digit

txout :: Parser TxOut
txout = TxOut <$> address <*> coin

balance :: Parser Command
balance = Balance <$> address

send :: Parser Command
send = Send <$> many1 txout

command :: Parser Command
command = try (text "balance") *> balance <|>
          try (text "send") *> send <|>
          try (text "quit") *> pure Quit <?>
          "Undefined command"

parseCommand :: Text -> Either String Command
parseCommand = first show . parse command ""
