{-# LANGUAGE ViewPatterns #-}

-- | Module for commands parsing

module Command
       ( Command (..)
       , parseCommand
       ) where

import           Data.String      (String)
import           Prelude          (read, show)
import           Text.Parsec      (many1, parse, try, (<?>))
import           Text.Parsec.Char (alphaNum, digit, spaces, string)
import           Text.Parsec.Text (Parser)
import           Universum        hiding (show)

import           Pos.Types        (Address (..), Coin (..), TxOut (..))

data Command = Balance Address
             | Send [TxOut]
             | Quit

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p

text :: String -> Parser Text
text = lexeme . fmap toText . string

address :: Parser Address
address = lexeme $ read <$> many1 alphaNum

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
