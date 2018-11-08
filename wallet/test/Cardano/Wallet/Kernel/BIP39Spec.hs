{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Kernel.BIP39Spec where

import           Universum

import           Cardano.Wallet.Kernel.BIP39
import           Crypto.Encoding.BIP39 (toEntropy)
import           Data.Default (def)
import           Pos.Crypto (AesKey (..))
import           Test.Hspec (Spec, describe, it, shouldBe, shouldReturn,
                     shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck ((===))

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL


-- | By default, private keys aren't comparable for security reasons (timing
-- attacks). We allow it here for testing purpose which is fine.
instance Eq CC.XPrv where
    (==) = (==) `on` CC.unXPrv

data TestVector = TestVector
    {
      -- | Raw JSON encoding (V1)
      bytes    :: BL.ByteString

      -- | Corresponding Entropy
    , entropy  :: Entropy (EntropySize 12)

      -- | Corresponding Mnemonic
    , mnemonic :: Mnemonic 12

      -- | Corresponding Seed
    , seed     :: ByteString

      -- | Corresponding AESKey
    , aesKey   :: AesKey
    }


spec :: Spec
spec = do
    prop "(9) entropyToMnemonic . mnemonicToEntropy == identity" $
        \e -> (mnemonicToEntropy @9 . entropyToMnemonic @9 @(EntropySize 9)) e == e

    prop "(12) entropyToMnemonic . mnemonicToEntropy == identity" $
        \e -> (mnemonicToEntropy @12 . entropyToMnemonic @12 @(EntropySize 12)) e == e

    prop "(9) parseJSON . toJSON == pure" $
        \(mw :: Mnemonic 9) -> (Aeson.decode . Aeson.encode) mw === pure mw

    prop "(12) parseJSON . toJSON == pure" $
        \(mw :: Mnemonic 12) -> (Aeson.decode . Aeson.encode) mw === pure mw

    describe "golden tests" $ do
        it "No example mnemonic" $
            mkMnemonic @12 defMnemonic `shouldSatisfy` isLeft

        it "No empty mnemonic" $
            mkMnemonic @12 [] `shouldSatisfy` isLeft

        it "No empty entropy" $
            mkEntropy @(EntropySize 12) "" `shouldSatisfy` isLeft

        it "Can generate 96 bits entropy" $
            (length . entropyToByteString <$> genEntropy @96) `shouldReturn` 12

        it "Can generate 128 bits entropy" $
            (length . entropyToByteString <$> genEntropy @128) `shouldReturn` 16

        it "Mnemonic to JSON" $ forM_ testVectors $ \TestVector{..} ->
            Aeson.encode mnemonic `shouldBe` bytes

        it "Mnemonic from JSON" $ forM_ testVectors $ \TestVector{..} ->
            Aeson.decode bytes `shouldBe` pure mnemonic

        it "Mnemonic to Entropy" $ forM_ testVectors $ \TestVector{..} ->
            mnemonicToEntropy mnemonic `shouldBe` entropy

        it "Mnemonic to Seed" $ forM_ testVectors $ \TestVector{..} ->
            mnemonicToSeed mnemonic `shouldBe` seed

        it "Mnemonic to Seed" $ forM_ testVectors $ \TestVector{..} ->
            mnemonicToAesKey mnemonic `shouldBe` aesKey

  where
    testVectors :: [TestVector]
    testVectors =
        [ TestVector "[\"abandon\",\"abandon\",\"abandon\",\"abandon\",\"abandon\",\"abandon\",\"abandon\",\"abandon\",\"abandon\",\"abandon\",\"abandon\",\"about\"]"
          (orFail $ mkEntropy'
              "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL")
          (orFail $ mkMnemonic
              ["abandon","abandon","abandon","abandon","abandon","abandon","abandon","abandon","abandon","abandon","abandon","about"])
          "X \223\238d\241\SI\212R\194\136)Q\239d\238\180\&8\128\170C\EOT\253\DC1\DC1\n/\ESC\DC3\145?%\138\157"
          $ AesKey "\148\193\192\136\204\148S\153gyc\n\211\175E\203\217(\DC4\130\141\215\132\207*\161-\249]\ESC\138\254"
        , TestVector "[\"letter\",\"advice\",\"cage\",\"absurd\",\"amount\",\"doctor\",\"acoustic\",\"avoid\",\"letter\",\"advice\",\"cage\",\"above\"]"
           (orFail $ mkEntropy'
             "\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128")
           (orFail $ mkMnemonic
             ["letter","advice","cage","absurd","amount","doctor","acoustic","avoid","letter","advice","cage","above"])
           "X \244o\161D\DC1Z\145qq\RS\237\r\130\206\DEL\159]:\DEL\135/\245`\n\222}\137\206y\194R\145"
           (AesKey "c\232\DC4?\DC2\199V\157\r\172\177x\141\&8\138\246l\174:^]9\249\192\149\230\180s\165\255,J")
        , TestVector
          "[\"zoo\",\"zoo\",\"zoo\",\"zoo\",\"zoo\",\"zoo\",\"zoo\",\"zoo\",\"zoo\",\"zoo\",\"zoo\",\"wrong\"]"
          (orFail $ mkEntropy'
            "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255")
          (orFail $ mkMnemonic
            ["zoo","zoo","zoo","zoo","zoo","zoo","zoo","zoo","zoo","zoo","zoo","wrong"])
          "X \SO\USL9R\238\128^u\167\246\197\215\SOH\RS\SI{\155\189m\177\212\254r\132\148\207K\134K\179{"
          (AesKey "h\DC4g\179\&3t%\253\&8\250\&9\131S\FS\161\166!M\233&N\235\171\223\156\155\197\209W\210\STX\180")
        ]
      where
        orFail :: Show e => Either e a -> a
        orFail =
            either (error . (<>) "Failed to create golden Mnemonic: " . show) identity
        mkEntropy' = toEntropy @128 @4 @ByteString

    defMnemonic :: [Text]
    defMnemonic = either (error . (<>) "Failed to encode/decode default menmonic " . show) identity
        $ Aeson.eitherDecode
        $ Aeson.encode
        $ def @(Mnemonic 12)
