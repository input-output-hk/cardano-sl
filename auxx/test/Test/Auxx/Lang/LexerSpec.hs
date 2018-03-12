module Test.Auxx.Lang.LexerSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Property, property)

import           Pos.Core (ApplicationName (..), BlockVersion (..), SoftwareVersion (..),
                           decodeTextAddress)
import           Pos.Crypto (decodeAbstractHash, parseFullPublicKey, unsafeCheatingHashCoerce)

import           Lang.Lexer (BracketSide (..), Token (..), detokenize, tokenize, tokenize')
import           Lang.Name (unsafeMkName)

spec :: Spec
spec = describe "Auxx.Lang.Lexer" $ do
    prop "accepts any input" propAcceptsAnyInput
    prop "handles valid input" propHandlesValidInput
    it "handles sample-1" unitLexerSample1
    it "handles sample-2" unitLexerSample2

propAcceptsAnyInput :: Property
propAcceptsAnyInput = property $ isJust . tokenize' . fromString

propHandlesValidInput :: Property
propHandlesValidInput = property $ liftA2 (==) (map snd . tokenize . detokenize) identity

unitLexerSample1 :: Expectation
unitLexerSample1 = map snd (tokenize input) `shouldBe` output
  where
    input  = " ( \"Hello\"; [=propose-patak-update ./secret.key /home/a_b\\ b-c] \"\\\"\"  ) "
    output =
        [ TokenParenthesis BracketSideOpening
        , TokenString "Hello"
        , TokenSemicolon
        , TokenSquareBracket BracketSideOpening
        , TokenEquals
        , TokenName $ unsafeMkName ["propose", "patak", "update"]
        , TokenFilePath "./secret.key"
        , TokenFilePath "/home/a_b b-c"
        , TokenSquareBracket BracketSideClosing
        , TokenString "\""
        , TokenParenthesis BracketSideClosing
        ]

unitLexerSample2 :: Expectation
unitLexerSample2 = map snd (tokenize input) `shouldBe` output
  where
    input =
        " oyGcGsd/FX3Zl98PPt/jE/mo+6Mz/HxaVcHxhrtxh6MrBkBi2U4h0pwaPDhWUo+IgcGzl4xLOqkoB4suojuNUA== \
        \ 5f53e01e1366aeda8811c2a630f0e037077a7b651093d2bdc4ef7200 \
        \ 04f2bf626c4e92d97683592c5af70ec243a5a5508a0bbb0adf7af49483cc9894 \
        \ 1.22.3 \
        \ ~software~cardano-sl:41 \
        \ Ae2tdPwUPEZ3Fd8HkQabvTJo3Ues7o2kNXXcK6LgGBfYwTM3pxpn5pijrBu \
        \ "
    output =
        [ TokenPublicKey . discardErrorText . parseFullPublicKey $
            "oyGcGsd/FX3Zl98PPt/jE/mo+6Mz/HxaVcHxhrtxh6MrBkBi2U4h0pwaPDhWUo+IgcGzl4xLOqkoB4suojuNUA=="
        , TokenStakeholderId . discardErrorText . decodeAbstractHash $
            "5f53e01e1366aeda8811c2a630f0e037077a7b651093d2bdc4ef7200"
        , TokenHash . unsafeCheatingHashCoerce . discardErrorText . decodeAbstractHash $
            "04f2bf626c4e92d97683592c5af70ec243a5a5508a0bbb0adf7af49483cc9894"
        , TokenBlockVersion $ BlockVersion 1 22 3
        , TokenSoftwareVersion $ SoftwareVersion
            { svAppName = UncheckedApplicationName "cardano-sl"
            , svNumber  = 41 }
        , TokenAddress . discardErrorText . decodeTextAddress $
            "Ae2tdPwUPEZ3Fd8HkQabvTJo3Ues7o2kNXXcK6LgGBfYwTM3pxpn5pijrBu"
        ]
    discardErrorText = either (\(_ :: Text) -> error "impossible") identity
