-- | Module for commands parsing

module Command
       ( Command (..)
       , parseCommand
       ) where

import qualified Data.List.NonEmpty         as NE
import           Prelude                    (read, show)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Parse        (parseIntegralSafe)
import           Text.Parsec                (many1, parse, try, (<?>))
import           Text.Parsec.Char           (alphaNum, anyChar, digit, space, spaces,
                                             string)
import           Text.Parsec.Combinator     (eof, manyTill)
import           Text.Parsec.Text           (Parser)
import           Universum                  hiding (show)

import           Pos.Binary                 ()
import           Pos.Core.Types             (ScriptVersion)
import           Pos.Core.Version           (parseBlockVersion, parseSoftwareVersion)
import           Pos.Crypto                 (Hash, decodeHash)
import           Pos.Txp                    (TxOut (..))
import           Pos.Types                  (Address (..), BlockVersion, EpochIndex,
                                             SoftwareVersion, decodeTextAddress, mkCoin)
import           Pos.Update                 (UpId)

data Command
    = Balance Address
    | Send Int (NonEmpty TxOut)
    | Vote Int Bool UpId
    | ProposeUpdate
          { puIdx             :: Int           -- TODO: what is this? rename
          , puBlockVersion    :: BlockVersion
          , puScriptVersion   :: ScriptVersion
          , puSlotDurationSec :: Int
          , puMaxBlockSize    :: Byte
          , puSoftwareVersion :: SoftwareVersion
          , puFilePath        :: Maybe FilePath
          }
    | Help
    | ListAddresses
    | DelegateLight !Int !Int
    | DelegateHeavy !Int !Int !(Maybe EpochIndex)
    | AddKeyFromPool !Int
    | AddKeyFromFile !FilePath
    | Quit
    deriving Show

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p >>= \x -> spaces $> x

text :: String -> Parser Text
text = lexeme . fmap toText . string

anyText :: Parser Text
anyText = lexeme $ fmap toText $ manyTill anyChar (void (try space) <|> try eof)

address :: Parser Address
address = lexeme $ do
    str <- many1 alphaNum
    case decodeTextAddress (toText str) of
        Left err -> fail (toString err)
        Right x  -> return x
-- pubKey :: Parser PublicKey
-- pubKey =
--     fromMaybe (panic "couldn't read pk") . parseFullPublicKey . toText <$>
--     lexeme (many1 alphaNum)

num :: Num a => Parser a
num = lexeme $ fromInteger . read <$> many1 digit

txout :: Parser TxOut
txout = TxOut <$> address <*> (mkCoin <$> num)

hash :: Parser (Hash a)
hash = decodeHash <$> anyText

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
delegateH = DelegateHeavy <$> num <*> num <*> optional num

addKeyFromPool, addKeyFromFile :: Parser Command
addKeyFromPool = AddKeyFromPool <$> num
addKeyFromFile = AddKeyFromFile <$> lexeme (many1 anyChar)

send :: Parser Command
send = Send <$> num <*> (NE.fromList <$> many1 txout)

vote :: Parser Command
vote = Vote <$> num <*> switch <*> hash

proposeUpdate :: Parser Command
proposeUpdate =
    ProposeUpdate <$>
    num <*>
    lexeme parseBlockVersion <*>
    lexeme parseIntegralSafe <*>
    lexeme parseIntegralSafe <*>
    lexeme parseIntegralSafe <*>
    lexeme parseSoftwareVersion <*>
    optional (lexeme (many1 anyChar))

command :: Parser Command
command = try (text "balance") *> balance <|>
          try (text "send") *> send <|>
          try (text "vote") *> vote <|>
          try (text "propose-update") *> proposeUpdate <|>
          try (text "delegate-light") *> delegateL <|>
          try (text "delegate-heavy") *> delegateH <|>
          try (text "add-key-pool") *> addKeyFromPool <|>
          try (text "add-key") *> addKeyFromFile <|>
          try (text "quit") *> pure Quit <|>
          try (text "help") *> pure Help <|>
          try (text "listaddr") *> pure ListAddresses <?>
          "Undefined command"

parseCommand :: Text -> Either String Command
parseCommand = first show . parse command ""
