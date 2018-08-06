-- | Module providing restoring from backup phrase functionality
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Util.Mnemonic
       (
       -- * Types
         Mnemonic
       , Entropy
       , EntropySize
       , MnemonicWords

       -- * Errors
       , MnemonicErr(..)
       , MnemonicException(..)

       -- * Creating @Mnemonic@ (resp. @Entropy@)
       , mkEntropy
       , mkMnemonic
       , genEntropy

       -- * Converting from and to @Mnemonic@ (resp. @Entropy@)
       , mnemonicToEntropy
       , mnemonicToSeed
       , mnemonicToAesKey
       , entropyToMnemonic
       , entropyToByteString
       ) where

import           Universum

import           Basement.Sized.List (unListN)
import           Control.Lens ((?~))
import           Crypto.Encoding.BIP39
import           Crypto.Encoding.BIP39.Dictionary (mnemonicSentenceToListN)
import           Crypto.Hash (Blake2b_256, Digest, hash)
import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Aeson.Types (Parser)
import           Data.ByteArray (constEq, convert)
import           Data.ByteString (ByteString)
import           Data.Default (Default (def))
import           Data.Swagger (NamedSchema (..), ToSchema (..), maxItems,
                     minItems)
import           Formatting (bprint, build, formatToString, (%))
import           Pos.Binary (serialize')
import           Pos.Crypto (AesKey (..))
import           Pos.Util.Log.LogSafe (SecureLog)
import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Gen (vectorOf)

import qualified Basement.Compat.Base as Basement
import qualified Basement.String as Basement
import qualified Basement.UArray as Basement
import qualified Crypto.Encoding.BIP39.English as Dictionary
import qualified Crypto.Random.Entropy as Crypto
import qualified Data.ByteString.Char8 as B8
import qualified Formatting.Buildable


--
-- TYPES
--

-- | A backup-phrase in the form of a non-empty of Mnemonic words
-- Constructor isn't exposed.
data Mnemonic (mw :: Nat) = Mnemonic
    { mnemonicToEntropy  :: Entropy (EntropySize mw)
    , mnemonicToSentence :: MnemonicSentence mw
    } deriving (Eq, Show)


--
-- ERRORS
--

data MnemonicException = UnexpectedMnemonicErr MnemonicErr
    deriving (Show, Typeable)


data MnemonicErr
    = MnemonicErrInvalidEntropyLength Int
    | MnemonicErrFailedToCreate
    | MnemonicErrForbiddenMnemonic
    deriving (Show)


--
-- CONSTRUCTORS
--

-- | Smart-constructor for the Entropy
mkEntropy
    :: forall n csz. (ValidEntropySize n, ValidChecksumSize n csz)
    => ByteString
    -> Either MnemonicErr (Entropy n)
mkEntropy =
    let
        n = fromIntegral $ natVal (Proxy @n)
    in
        maybe (Left $ MnemonicErrInvalidEntropyLength n) Right . toEntropy @n


-- | Generate Entropy of a given size using a random seed.
--
-- Example:
--     do
--       ent <- genEntropy :: IO (Entropy 12)
genEntropy
    :: forall n csz. (ValidEntropySize n, ValidChecksumSize n csz)
    => IO (Entropy n)
genEntropy =
    let
        size =
            fromIntegral $ natVal (Proxy @n)
        eitherToIO =
            either (throwM . UnexpectedMnemonicErr) return
    in
        (eitherToIO . mkEntropy) =<< Crypto.getEntropy (size `div` 8)


-- | Smart-constructor for the Mnemonic
mkMnemonic
    :: forall mw n csz.
     ( ConsistentEntropy n mw csz
     , EntropySize mw ~ n
     )
    => [Text]
    -> Either MnemonicErr (Mnemonic mw)
mkMnemonic wordsm = do
    sentence <- maybe
        (Left MnemonicErrFailedToCreate)
        (Right . mnemonicPhraseToMnemonicSentence Dictionary.english)
        (mnemonicPhrase @mw (toUtf8String <$> wordsm))

    entropy <- maybe
        (Left MnemonicErrFailedToCreate)
        Right
        (wordsToEntropy sentence :: Maybe (Entropy n))

    when (isForbiddenMnemonic sentence) $
        Left MnemonicErrForbiddenMnemonic

    pure $ Mnemonic
        { mnemonicToEntropy  = entropy
        , mnemonicToSentence = sentence
        }


--
-- CONVERSIONS
--

-- | Convert a mnemonic to a seed that can be used to initiate a HD wallet.
-- Note that our current implementation deviates from BIP-39 as:
--
--    - We do not use the password to produce the seed
--    - We rely on a fast blake2b hashing function rather than a slow PKBDF2
--
-- Somehow, we also convert mnemonic to raw bytes using a Blake2b_256 but with
-- a slightly different approach when converting them to aesKey when redeeming
-- paper wallets... In this case, we do not serialize the inputs and outputs.
--
-- For now, we have two use case for that serialization function. When creating
-- an HD wallet seed, in which case, the function we use is `serialize'` from
-- the Pos.Binary module. And, when creating an AESKey seed in which case we
-- simply pass the `identity` function.
mnemonicToSeed
    :: Mnemonic mw
    -> ByteString
mnemonicToSeed =
    serialize' . blake2b . serialize' . entropyToByteString . mnemonicToEntropy


-- | Convert a mnemonic to a seed AesKey. Almost identical to @MnemonictoSeed@
-- minus the extra serialization.
mnemonicToAesKey
    :: Mnemonic mw
    -> AesKey
mnemonicToAesKey =
    AesKey. blake2b . entropyToByteString . mnemonicToEntropy


-- | Convert an Entropy to a corresponding Mnemonic Sentence
entropyToMnemonic
    :: forall mw n csz.
     ( ValidMnemonicSentence mw
     , ValidEntropySize n
     , ValidChecksumSize n csz
     , n ~ EntropySize mw
     , mw ~ MnemonicWords n
     )
    => Entropy n
    -> Mnemonic mw
entropyToMnemonic entropy = Mnemonic
    { mnemonicToSentence = entropyToWords entropy
    , mnemonicToEntropy  = entropy
    }


-- | Convert 'Entropy' to a raw 'ByteString'
entropyToByteString
    :: Entropy n
    -> ByteString
entropyToByteString =
    entropyRaw


--
-- INTERNALS
--

-- Constant-time comparison of any sentence with the 12-word example mnemonic
isForbiddenMnemonic :: (ValidMnemonicSentence mw) => MnemonicSentence mw -> Bool
isForbiddenMnemonic sentence =
    let
        bytes =
            sentenceToRawString sentence

        forbiddenMnemonics = sentenceToRawString <$>
            [ mnemonicToSentence (def @(Mnemonic 12))
            ]
    in
        any (constEq bytes) forbiddenMnemonics


sentenceToRawString :: (ValidMnemonicSentence mw) => MnemonicSentence mw -> Basement.UArray Word8
sentenceToRawString =
    Basement.toBytes Basement.UTF8 . mnemonicSentenceToString Dictionary.english


-- | Simple Blake2b 256-bit of a ByteString
blake2b :: ByteString -> ByteString
blake2b =
    convert @(Digest Blake2b_256) . hash


toUtf8String :: Text -> Basement.String
toUtf8String =
    Basement.fromString . toString


fromUtf8String :: Basement.String -> Text
fromUtf8String =
    toText . Basement.toList


--- | The initial seed has to be vector or length multiple of 4 bytes and shorter
--- than 64 bytes. Not that this is good for testing or examples, but probably
--- not for generating truly random Mnemonic words.
---
--- See 'Crypto.Random.Entropy (getEntropy)'
instance
    ( ValidEntropySize n
    , ValidChecksumSize n csz
    ) => Arbitrary (Entropy n) where
    arbitrary =
        let
            size    = fromIntegral $ natVal (Proxy @n)
            entropy = mkEntropy  @n . B8.pack <$> vectorOf (size `quot` 8) arbitrary
        in
            either (error . show . UnexpectedMnemonicErr) identity <$> entropy


-- Same remark from 'Arbitrary Entropy' applies here.
instance
    ( n ~ EntropySize mw
    , mw ~ MnemonicWords n
    , ValidChecksumSize n csz
    , ValidEntropySize n
    , ValidMnemonicSentence mw
    , Arbitrary (Entropy n)
    ) => Arbitrary (Mnemonic mw) where
    arbitrary =
        entropyToMnemonic <$> arbitrary @(Entropy n)


instance Exception MnemonicException


-- FIXME: Suggestion, we could -- when certain flags are turned on -- display
-- a fingerprint of the Mnemonic, like a PKBDF2 over n iterations. This could be
-- useful for debug to know whether two users are using the same mnemonic words
-- and relatively benign EVEN THOUGH, it will permit to somewhat tight requests
-- to a specific identity (since mnemonic words are 'unique', they are supposed
-- to uniquely identify users, hence the privacy issue). For debbugging only and
-- with the user consent, that's something we could do.
instance Buildable (Mnemonic mw) where
    build _ =
        "<mnemonic>"

instance Buildable (SecureLog (Mnemonic mw)) where
    build _ =
        "<mnemonic>"

instance Buildable MnemonicErr where
    build = \case
        MnemonicErrInvalidEntropyLength l ->
            bprint ("Entropy must be a sequence of " % build % " bytes") l
        MnemonicErrFailedToCreate ->
            bprint "Invalid Mnemonic words"
        MnemonicErrForbiddenMnemonic ->
            bprint "Forbidden Mnemonic: an example Mnemonic has been submitted. \
            \Please generate a fresh and private Mnemonic from a trusted source"


-- | To use everytime we need to show an example of a Mnemonic. This particular
-- mnemonic is rejected to prevent users from using it on a real wallet.
instance Default (Mnemonic 12) where
    def =
        let
            wordsm =
                [ "squirrel"
                , "material"
                , "silly"
                , "twice"
                , "direct"
                , "slush"
                , "pistol"
                , "razor"
                , "become"
                , "junk"
                , "kingdom"
                , "flee"
                ]

            sentence = maybe
                (error $ show $ UnexpectedMnemonicErr MnemonicErrFailedToCreate)
                (mnemonicPhraseToMnemonicSentence Dictionary.english)
                (mnemonicPhrase @12 (toUtf8String <$> wordsm))

            entropy = fromMaybe
                (error $ show $ UnexpectedMnemonicErr MnemonicErrFailedToCreate)
                (wordsToEntropy @(EntropySize 12) sentence)
        in Mnemonic
            { mnemonicToSentence = sentence
            , mnemonicToEntropy  = entropy
            }


instance
    ( n ~ EntropySize mw
    , mw ~ MnemonicWords n
    , ValidChecksumSize n csz
    , ValidEntropySize n
    , ValidMnemonicSentence mw
    , Arbitrary (Entropy n)
    ) => FromJSON (Mnemonic mw) where
    parseJSON =
        parseJSON >=> (eitherToParser . mkMnemonic)
      where
        eitherToParser :: Either MnemonicErr (Mnemonic mw) -> Parser (Mnemonic mw)
        eitherToParser =
            either (fail . formatToString build) pure


instance ToJSON (Mnemonic mw) where
    toJSON =
        toJSON
        . map (fromUtf8String . dictionaryIndexToWord Dictionary.english)
        . unListN
        . mnemonicSentenceToListN
        . mnemonicToSentence


instance (KnownNat mw) => ToSchema (Mnemonic mw) where
    declareNamedSchema _ = do
        let mw = natVal (Proxy :: Proxy mw)
        NamedSchema _ schema <- declareNamedSchema (Proxy @[Text])
        return $ NamedSchema (Just "Mnemonic") schema
            & minItems ?~ fromIntegral mw
            & maxItems ?~ fromIntegral mw
