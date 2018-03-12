module Lang.Lexer
       ( BracketSide(..)
       , _BracketSideOpening
       , _BracketSideClosing
       , UnknownChar(..)
       , FilePath'(..)
       , Token(..)
       , _TokenSquareBracket
       , _TokenParenthesis
       , _TokenString
       , _TokenAddress
       , _TokenPublicKey
       , _TokenStakeholderId
       , _TokenHash
       , _TokenBlockVersion
       , _TokenSoftwareVersion
       , _TokenFilePath
       , _TokenNumber
       , _TokenName
       , _TokenKey
       , _TokenEquals
       , _TokenSemicolon
       , _TokenUnknown
       , tokenize
       , tokenize'
       , detokenize
       ) where

import           Universum

import qualified Control.Applicative.Combinators.NonEmpty as NonEmpty
import           Control.Lens (makePrisms)
import           Data.Char (isAlpha, isAlphaNum)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Loc (Loc, Span, loc, spanFromTo)
import           Data.Scientific (Scientific)
import qualified Data.Text as Text
import qualified Data.Text.Buildable as Buildable
import           Formatting (sformat)
import           Test.QuickCheck.Arbitrary.Generic (Arbitrary (..), genericArbitrary, genericShrink)
import qualified Test.QuickCheck.Gen as QC
import           Test.QuickCheck.Instances ()
import           Text.Megaparsec (Parsec, SourcePos (..), between, choice, eof, getPosition,
                                  manyTill, notFollowedBy, parseMaybe, skipMany, takeP, takeWhile1P,
                                  try, unPos, (<?>))
import           Text.Megaparsec.Char (anyChar, char, satisfy, spaceChar, string)
import           Text.Megaparsec.Char.Lexer (charLiteral, decimal, scientific, signed)

import           Lang.Name (Letter, Name (..), unsafeMkLetter)
import           Pos.Arbitrary.Core ()
import           Pos.Core (Address, ApplicationName (..), BlockVersion (..), SoftwareVersion (..),
                           StakeholderId, decodeTextAddress)
import           Pos.Crypto (AHash (..), PublicKey, decodeAbstractHash, fullPublicKeyF, hashHexF,
                             parseFullPublicKey, unsafeCheatingHashCoerce)
import           Pos.Util.Util (toParsecError)

data BracketSide = BracketSideOpening | BracketSideClosing
    deriving (Eq, Ord, Show, Generic)

makePrisms ''BracketSide

withBracketSide :: a -> a -> BracketSide -> a
withBracketSide onOpening onClosing = \case
    BracketSideOpening -> onOpening
    BracketSideClosing -> onClosing

instance Arbitrary BracketSide where
    arbitrary = genericArbitrary
    shrink = genericShrink

newtype UnknownChar = UnknownChar Char
    deriving (Eq, Ord, Show)

instance Arbitrary UnknownChar where
    arbitrary = pure (UnknownChar '\0')

newtype FilePath' = FilePath'
    { getFilePath' :: FilePath
    } deriving (Eq, Ord, Show, Generic, IsString)

instance Arbitrary FilePath' where
    arbitrary = QC.elements
        [ "/a/b/c"
        , "./a/b/c"
        , "/p a t h/h e r e.k"
        ] -- TODO: proper generator

instance Buildable FilePath' where
    build = fromString . concatMap escape . getFilePath'
      where
        escape c | isFilePathChar c = [c]
                 | otherwise = '\\':[c]

isFilePathChar :: Char -> Bool
isFilePathChar c = isAlphaNum c || c `elem` ['.', '/', '-', '_']

data Token
    = TokenSquareBracket BracketSide
    | TokenParenthesis BracketSide
    | TokenString String
    | TokenNumber Scientific
    | TokenAddress Address
    | TokenPublicKey PublicKey
    | TokenStakeholderId StakeholderId
    | TokenHash AHash
    | TokenBlockVersion BlockVersion
    | TokenSoftwareVersion SoftwareVersion
    | TokenFilePath FilePath'
    | TokenName Name
    | TokenKey Name
    | TokenEquals
    | TokenSemicolon
    | TokenUnknown UnknownChar
    deriving (Eq, Ord, Show, Generic)

makePrisms ''Token

instance Arbitrary Token where
    arbitrary = genericArbitrary
    shrink = genericShrink

tokenRender :: Token -> Text
tokenRender = \case
    TokenSquareBracket bs -> withBracketSide "[" "]" bs
    TokenParenthesis bs -> withBracketSide "(" ")" bs
    TokenString s -> show s
    TokenNumber n -> show n
    TokenAddress a -> pretty a
    TokenPublicKey pk -> sformat fullPublicKeyF pk
    TokenStakeholderId sId -> sformat hashHexF sId
    TokenHash h -> sformat hashHexF (getAHash h)
    TokenBlockVersion v -> pretty v
    TokenSoftwareVersion v -> "~software~" <> pretty v
    TokenFilePath s -> pretty s
    TokenName ss -> pretty ss
    TokenKey ss -> pretty ss <> ":"
    TokenEquals -> "="
    TokenSemicolon -> ";"
    TokenUnknown (UnknownChar c) -> Text.singleton c

detokenize :: [Token] -> Text
detokenize = unwords . List.map tokenRender

type Lexer a = Parsec Void Text a

tokenize :: Text -> [(Span, Token)]
tokenize = fromMaybe noTokenErr . tokenize'
  where
    noTokenErr =
        error "tokenize: no token could be consumed. This is a bug"

tokenize' :: Text -> Maybe [(Span, Token)]
tokenize' = parseMaybe (between pSkip eof (many pToken))

pToken :: Lexer (Span, Token)
pToken = withPosition (try pToken' <|> pUnknown) <* pSkip
  where
    posToLoc :: SourcePos -> Loc
    posToLoc SourcePos{..} = uncurry loc
        ( fromIntegral . unPos $ sourceLine
        , fromIntegral . unPos $ sourceColumn)
    withPosition p = do
        pos1 <- posToLoc <$> getPosition
        t <- p
        pos2 <- posToLoc <$> getPosition
        return (spanFromTo pos1 pos2, t)

pUnknown :: Lexer Token
pUnknown = TokenUnknown . UnknownChar <$> anyChar

pSkip :: Lexer ()
pSkip = skipMany (void spaceChar)

marking :: Text -> Lexer a -> Lexer a
marking t p = optional (string $ "~" <> t <> "~") *> p

pToken' :: Lexer Token
pToken' = choice
    [ pPunct
    , marking "addr" $ TokenAddress <$> try pAddress
    , marking "pk" $ TokenPublicKey <$> try pPublicKey
    , marking "stakeholder" $ TokenStakeholderId <$> try pStakeholderId
    , marking "hash" $ TokenHash <$> try pHash
    , marking "block-v" $ TokenBlockVersion <$> try pBlockVersion
    , string "~software~" *> (TokenSoftwareVersion <$> try pSoftwareVersion)
    , marking "filepath" $ TokenFilePath <$> pFilePath
    , marking "num" $ TokenNumber <$> pScientific
    , marking "str" $ TokenString <$> pString
    , marking "ident" $ pIdent
    ] <?> "token"

pPunct :: Lexer Token
pPunct = choice
    [ char '[' $> TokenSquareBracket BracketSideOpening
    , char ']' $> TokenSquareBracket BracketSideClosing
    , char '(' $> TokenParenthesis BracketSideOpening
    , char ')' $> TokenParenthesis BracketSideClosing
    , char '=' $> TokenEquals
    , char ';' $> TokenSemicolon
    ] <?> "punct"

pString :: Lexer String
pString =
    char '\"' *>
    manyTill (charLiteral <|> anyChar) (char '\"')

pSomeAlphaNum :: Lexer Text
pSomeAlphaNum = takeWhile1P (Just "alphanumeric") isAlphaNum

pAddress :: Lexer Address
pAddress = do
    str <- pSomeAlphaNum
    toParsecError $ decodeTextAddress str

pPublicKey :: Lexer PublicKey
pPublicKey = do
    str <- (<>) <$> takeP (Just "base64") 86 <*> string "=="
    toParsecError $ parseFullPublicKey str

pStakeholderId :: Lexer StakeholderId
pStakeholderId = do
    str <- pSomeAlphaNum
    toParsecError $ decodeAbstractHash str

pHash :: Lexer AHash
pHash = do
    str <- pSomeAlphaNum
    toParsecError . fmap unsafeCheatingHashCoerce $ decodeAbstractHash str

pBlockVersion :: Lexer BlockVersion
pBlockVersion = do
    bvMajor <- decimal
    void $ char '.'
    bvMinor <- decimal
    void $ char '.'
    bvAlt <- decimal
    notFollowedBy $ char '.'
    return BlockVersion{..}

pSoftwareVersion :: Lexer SoftwareVersion
pSoftwareVersion = do
    appName <- manyTill (satisfy isAlphaNum <|> char '-') (char ':')
    let svAppName = UncheckedApplicationName (toText appName)
    svNumber <- decimal
    notFollowedBy $ char '.'
    return SoftwareVersion {..}

pFilePath :: Lexer FilePath'
pFilePath = FilePath' <$> do
    dots <- many (char '.')
    cs <-
        (:) <$> char '/'
            <*> many pFilePathChar
        <|> pure ""
    notFollowedBy pFilePathChar
    let path = dots <> cs
    guard $ not (null path)
    return path
  where
    pFilePathChar :: Lexer Char
    pFilePathChar =
        char '\\' *> anyChar <|>
        satisfy isFilePathChar

pIdent :: Lexer Token
pIdent = do
    name <- NonEmpty.sepBy1 pNameSection (char '-')
    notFollowedBy (satisfy isAlphaNum)
    isKey <- isJust <$> optional (char ':')
    return $ (if isKey then TokenKey else TokenName) (Name name)

pNameSection :: Lexer (NonEmpty Letter)
pNameSection = NonEmpty.some1 pLetter

pLetter :: Lexer Letter
pLetter = unsafeMkLetter <$> satisfy isAlpha

pScientific :: Lexer Scientific
pScientific = do
    n <- signed (return ()) scientific
    p <- isJust <$> optional (char '%')
    return $ if p then n / 100 else n
