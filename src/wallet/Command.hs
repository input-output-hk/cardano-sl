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

import           Pos.Types        (Address (..), TxOut (..), mkCoin)

data Command
    = Balance Address
    | Send Int [TxOut]
    | Help
    | ListAddresses
    | DelegateLight !Int !Int
    | DelegateHeavy !Int !Int
    | Quit
    deriving Show

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p

text :: String -> Parser Text
text = lexeme . fmap toText . string

address :: Parser Address
address = lexeme $ read <$> many1 alphaNum

-- pubKey :: Parser PublicKey
-- pubKey =
--     fromMaybe (panic "couldn't read pk") . parseFullPublicKey . toText <$>
--     lexeme (many1 alphaNum)

num :: Num a => Parser a
num = lexeme $ fromInteger . read <$> many1 digit

txout :: Parser TxOut
txout = TxOut <$> address <*> (mkCoin <$> num)

balance :: Parser Command
balance = Balance <$> address

delegateL, delegateH :: Parser Command
delegateL = DelegateLight <$> num <*> num
delegateH = DelegateHeavy <$> num <*> num

send :: Parser Command
send = Send <$> num <*> many1 txout

command :: Parser Command
command = try (text "balance") *> balance <|>
          try (text "send") *> send <|>
          try (text "delegate-light") *> delegateL <|>
          try (text "delegate-heavy") *> delegateH <|>
          try (text "quit") *> pure Quit <|>
          try (text "help") *> pure Help <|>
          try (text "listaddr") *> pure ListAddresses <?>
          "Undefined command"

parseCommand :: Text -> Either String Command
parseCommand = first show . parse command ""
