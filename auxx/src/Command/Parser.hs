-- | Module for commands parsing.

module Command.Parser
       ( parseCommand
       ) where

import           Universum

import           Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58)
import qualified Data.List.NonEmpty     as NE
import qualified Data.Map               as Map
import qualified Data.Text              as Text
import           Prelude                (read)
import           Serokell.Util.Parse    (parseIntegralSafe)
import           Text.Parsec            (many1, parse, parserFail, try, (<?>))
import           Text.Parsec.Char       (alphaNum, anyChar, digit, noneOf, space, spaces,
                                         string)
import           Text.Parsec.Combinator (eof, manyTill)
import           Text.Parsec.Text       (Parser)

import           Pos.Binary             (decodeFull)
import           Pos.Client.CLI         (stakeholderIdParser)
import           Pos.Core.Version       (parseBlockVersion, parseSoftwareVersion)
import           Pos.Crypto             (AbstractHash, HashAlgorithm, PublicKey,
                                         decodeAbstractHash)
import           Pos.Txp                (TxOut (..))
import           Pos.Types              (AddrStakeDistribution (..), Address, Coin,
                                         CoinPortion, decodeTextAddress, mkCoin,
                                         mkMultiKeyDistr, unsafeCoinPortionFromDouble)
import           Pos.Update             (SystemTag, mkSystemTag)
import           Pos.Util.Util          (eitherToFail)

import           Command.Types          (Command (..), ProposeUpdateParams (..),
                                         ProposeUpdateSystem (..), SendMode (..),
                                         SendToAllGenesisParams (..))


lexeme :: Parser a -> Parser a
lexeme p = spaces *> p >>= \x -> spaces $> x

text :: String -> Parser Text
text = lexeme . fmap toText . string

anyText :: Parser Text
anyText = lexeme $ fmap toText $ manyTill anyChar (void (try space) <|> try eof)

filePath :: Parser FilePath
filePath = lexeme (many1 nonSpace)
  where
    nonSpace = noneOf [' ']

address :: Parser Address
address = lexeme $ do
    str <- many1 alphaNum
    case decodeTextAddress (toText str) of
        Left err -> fail (toString err)
        Right x  -> return x

num :: Num a => Parser a
num = lexeme $ fromInteger . read <$> many1 digit

coin :: Parser Coin
coin = mkCoin <$> num

txout :: Parser TxOut
txout = TxOut <$> address <*> coin

hash :: (HashAlgorithm algo, Typeable a) => Parser (AbstractHash algo a)
hash = eitherToFail . decodeAbstractHash =<< lexeme (toText <$> many1 alphaNum)

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
    eitherToFail (decodeFull bs)

delegateL, delegateH :: Parser Command
delegateL = DelegateLight <$> num <*> base58PkParser <*> num <*> optional num
delegateH = DelegateHeavy <$> num <*> base58PkParser <*> num

addKeyFromPool, addKeyFromFile :: Parser Command
addKeyFromPool = AddKeyFromPool <$> num
addKeyFromFile = AddKeyFromFile <$> filePath

send :: Parser Command
send = Send <$> num <*> (NE.fromList <$> many1 txout)

sendMode :: Parser SendMode
sendMode = lexeme $ text "neighbours" $> SendNeighbours
                <|> text "round-robin" $> SendRoundRobin
                <|> text "send-random" $> SendRandom

sendToAllGenesisParams :: Parser SendToAllGenesisParams
sendToAllGenesisParams =
    SendToAllGenesisParams <$> num <*> num <*> num <*> sendMode <*> filePath

sendToAllGenesis :: Parser Command
sendToAllGenesis = SendToAllGenesis <$> sendToAllGenesisParams

vote :: Parser Command
vote = Vote <$> num <*> switch <*> hash

proposeUpdateParams :: Parser ProposeUpdateParams
proposeUpdateParams =
    ProposeUpdateParams <$>
    num <*>
    lexeme parseBlockVersion <*>
    lexeme parseIntegralSafe <*>
    lexeme parseIntegralSafe <*>
    lexeme parseIntegralSafe <*>
    lexeme parseSoftwareVersion <*>
    many1 parseProposeUpdateSystem

proposeUpdate :: Parser Command
proposeUpdate = ProposeUpdate <$> proposeUpdateParams

coinPortionP :: Parser CoinPortion
coinPortionP = do
    (token, modifier) <- anyText <&> \s -> case Text.stripSuffix "%" s of
        Nothing -> (s, identity)
        Just s' -> (s', (/100))
    case readMaybe @Double (toString token) of
        Just (modifier -> a) | a >= 0, a <= 1 -> return $ unsafeCoinPortionFromDouble a
        _                    -> fail "Expected a coin portion"

addrStakeDistrP :: Parser AddrStakeDistribution
addrStakeDistrP =
    BootstrapEraDistr <$ text "boot" <|>
    multiKeyDistrP
  where
    multiKeyDistrP = do
        parts <- many1 ((,) <$> stakeholderIdParser <* text ":" <*> coinPortionP)
        case parts of
            [(sId, coinPortion)] | coinPortion == maxBound -> return $ SingleKeyDistr sId
            _ -> eitherToFail $ mkMultiKeyDistr (Map.fromList parts)

addrDistrP :: Parser Command
addrDistrP = AddrDistr <$> lexeme base58PkParser <*> lexeme addrStakeDistrP

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
          try (text "addr-distr") *> addrDistrP <|>
          try (text "quit") *> pure Quit <|>
          try (text "help") *> pure Help <|>
          try (text "listaddr") *> pure ListAddresses <?>
          "Undefined command"

parseCommand :: Text -> Either String Command
parseCommand = first show . parse command ""
