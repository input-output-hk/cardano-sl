-- | Module for commands parsing

module Command
       ( Command (..)
       , parseCommand
       ) where

import           Prelude                (read, show)
import           Text.Parsec            (many1, parse, try, (<?>))
import           Text.Parsec.Char       (alphaNum, anyChar, digit, space, spaces, string)
import           Text.Parsec.Combinator (eof, manyTill)
import           Text.Parsec.Text       (Parser)
import           Universum              hiding (show)

import           Pos.Crypto             (Hash, parseHash)
import           Pos.Script.Type        (ScriptVersion)
import           Pos.Types              (Address (..), ProtocolVersion, SoftwareVersion,
                                         TxOut (..), mkCoin, parseProtocolVersion,
                                         parseSoftwareVersion)
import           Pos.Update             (UpId)
import           Pos.Util               (parseIntegralSafe)

data Command
    = Balance Address
    | Send Int [TxOut]
    | Vote Int Bool UpId
    | ProposeUpdate Int ProtocolVersion ScriptVersion SoftwareVersion UpId
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

anyText :: Parser Text
anyText = lexeme $ fmap toText $ manyTill anyChar (void (try space) <|> try eof)

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

hash :: Parser (Hash a)
hash = handleRes . parseHash =<< anyText
  where
    handleRes (Left err) = fail (show err)
    handleRes (Right x)  = return x

switch :: Parser Bool
switch = lexeme $ positive $> True <|>
                  negative $> False
  where
    positive = text "+" <|> text "y" <|> text "yes"
    negative = text "-" <|> text "n" <|> text "no"

balance :: Parser Command
balance = Balance <$> address

delegateL, delegateH :: Parser Command
delegateL = DelegateLight <$> num <*> num
delegateH = DelegateHeavy <$> num <*> num

send :: Parser Command
send = Send <$> num <*> many1 txout

vote :: Parser Command
vote = Vote <$> num <*> switch <*> hash

proposeUpdate :: Parser Command
proposeUpdate =
    ProposeUpdate <$>
    num <*>
    lexeme parseProtocolVersion <*>
    lexeme parseIntegralSafe    <*>
    lexeme parseSoftwareVersion <*>
    hash

command :: Parser Command
command = try (text "balance") *> balance <|>
          try (text "send") *> send <|>
          try (text "vote") *> vote <|>
          try (text "propose-update") *> proposeUpdate <|>
          try (text "delegate-light") *> delegateL <|>
          try (text "delegate-heavy") *> delegateH <|>
          try (text "quit") *> pure Quit <|>
          try (text "help") *> pure Help <|>
          try (text "listaddr") *> pure ListAddresses <?>
          "Undefined command"

parseCommand :: Text -> Either String Command
parseCommand = first show . parse command ""
