module Pos.Core.Common.Script
       ( Script (..)
       , ScriptVersion
       , Script_v0
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (toJSON), object, withObject,
                     (.:), (.=))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, int, (%))
import qualified Formatting.Buildable as Buildable
import qualified PlutusCore.Program as PLCore
import           Serokell.Util.Base64 (JsonByteString (..))

import qualified Pos.Binary.Class as Bi

-- | Version of script
type ScriptVersion = Word16

-- | A script for inclusion into a transaction.
data Script = Script
    { scrVersion :: ScriptVersion -- ^ Version
    , scrScript  :: ByteString   -- ^ Serialized script
    } deriving (Eq, Show, Generic, Typeable)

instance NFData Script
instance Hashable Script

instance Buildable Script where
    build Script{..} = bprint ("<script v"%int%">") scrVersion

instance ToJSON Script where
    toJSON Script{..} = object [
        "version"    .= scrVersion,
        "script" .= JsonByteString scrScript ]

instance FromJSON Script where
    parseJSON = withObject "Script" $ \obj -> do
        scrVersion <- obj .: "version"
        scrScript  <- getJsonByteString <$> obj .: "script"
        pure $ Script {..}

Bi.deriveSimpleBi ''Script [
    Bi.Cons 'Script [
        Bi.Field [| scrVersion :: ScriptVersion |],
        Bi.Field [| scrScript  :: ByteString   |]
    ]]

deriveSafeCopySimple 0 'base ''Script

-- | Deserialized script (i.e. an AST), version 0.
type Script_v0 = PLCore.Program
