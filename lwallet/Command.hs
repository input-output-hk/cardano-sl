-- | Module for commands parsing

module Command
       ( Command (..)
       , ProposeUpdateSystem (..)
       , SendMode (..)
       , parseCommand
       ) where

import           Data.ByteString.Base58     (bitcoinAlphabet, decodeBase58)
import qualified Data.List.NonEmpty         as NE
import           Prelude                    (read, show)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Parse        (parseIntegralSafe)
import           Text.Parsec                (many1, parse, parserFail, try, (<?>))
import           Text.Parsec.Char           (alphaNum, anyChar, digit, noneOf, oneOf,
                                             space, spaces, string)
import           Text.Parsec.Combinator     (eof, manyTill)
import           Text.Parsec.Text           (Parser)
import           Universum                  hiding (show)

import           Pos.Binary                 (decodeOrFail)
import           Pos.Core.Types             (ScriptVersion)
import           Pos.Core.Version           (parseBlockVersion, parseSoftwareVersion)
import           Pos.Crypto                 (Hash, PublicKey, decodeHash)
import           Pos.Txp                    (TxOut (..))
import           Pos.Types                  (Address (..), BlockVersion, Coin, EpochIndex,
                                             SoftwareVersion, decodeTextAddress, mkCoin)
import           Pos.Update                 (SystemTag, UpId, mkSystemTag)

data SendMode = SendNeighbours | SendRoundRobin | SendRandom deriving Show

data Command
    = Balance Address
    | Send Int (NonEmpty TxOut)
    | SendToAllGenesis !Int !Int !Int !Int !SendMode !FilePath
    | Vote Int Bool UpId
    | ProposeUpdate
          { puIdx             :: Int           -- TODO: what is this? rename
          , puBlockVersion    :: BlockVersion
          , puScriptVersion   :: ScriptVersion
          , puSlotDurationSec :: Int
          , puMaxBlockSize    :: Byte
          , puSoftwareVersion :: SoftwareVersion
          , puUpdates         :: [ProposeUpdateSystem]
          }
    | Help
    | ListAddresses
    | DelegateLight !Int !PublicKey !EpochIndex !(Maybe EpochIndex) -- first and last epoch of psk ttl
    | DelegateHeavy !Int !PublicKey !EpochIndex -- last argument is current epoch
    | AddKeyFromPool !Int
    | AddKeyFromFile !FilePath
    | Quit
    deriving Show

data ProposeUpdateSystem = ProposeUpdateSystem
    { pusSystemTag     :: SystemTag
    , pusInstallerPath :: Maybe FilePath
    , pusBinDiffPath   :: Maybe FilePath
    } deriving Show

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p >>= \x -> spaces $> x

text :: String -> Parser Text
text = lexeme . fmap toText . string

many1Till :: Show b => Parser a -> Parser b -> Parser [a]
many1Till p end = (:) <$> p <*> manyTill p end

anyText :: Parser Text
anyText = lexeme $ fmap toText $ manyTill anyChar (void (try space) <|> try eof)

filePath :: Parser FilePath
filePath = many1Till (alphaNum <|> oneOf "_.") (void (try space) <|> try eof)

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

coin :: Parser Coin
coin = mkCoin <$> num

txout :: Parser TxOut
txout = TxOut <$> address <*> coin

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

base58PkParser :: Parser PublicKey
base58PkParser = do
    token <- some $ noneOf " "
    bs <- maybe (fail "Incorrect base58") pure $
        decodeBase58 bitcoinAlphabet (encodeUtf8 $ toText token)
    pure (decodeOrFail bs)

delegateL, delegateH :: Parser Command
delegateL = DelegateLight <$> num <*> base58PkParser <*> num <*> optional num
delegateH = DelegateHeavy <$> num <*> base58PkParser <*> num

addKeyFromPool, addKeyFromFile :: Parser Command
addKeyFromPool = AddKeyFromPool <$> num
addKeyFromFile = AddKeyFromFile <$> lexeme (many1 anyChar)

send :: Parser Command
send = Send <$> num <*> (NE.fromList <$> many1 txout)

sendMode :: Parser SendMode
sendMode = lexeme $ text "neighbours" $> SendNeighbours
                <|> text "round-robin" $> SendRoundRobin
                <|> text "send-random" $> SendRandom

sendToAllGenesis :: Parser Command
sendToAllGenesis = SendToAllGenesis <$> num <*> num <*> num <*> num <*> sendMode <*> lexeme (many1 anyChar)

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
    many1 parseProposeUpdateSystem

parseProposeUpdateSystem :: Parser ProposeUpdateSystem
parseProposeUpdateSystem =
    ProposeUpdateSystem <$>
    lexeme parseSystemTag <*>
    lexeme parseOptionalFilePath <*>
    lexeme parseOptionalFilePath

parseOptionalFilePath :: Parser (Maybe FilePath)
parseOptionalFilePath = (text "none" $> Nothing)
                    <|> (filePath >>= pure . Just)

parseSystemTag :: Parser SystemTag
parseSystemTag =
    either parserFail pure . mkSystemTag . toText =<<
        (many alphaNum)

command :: Parser Command
command = try (text "balance") *> balance <|>
          try (text "send-to-all-genesis") *> sendToAllGenesis <|>
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
