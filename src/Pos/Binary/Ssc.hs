{-# LANGUAGE LambdaCase #-}

-- | GodTossing serialization instances

module Pos.Binary.Ssc () where

import           Universum

import           Pos.Binary.Class                (Bi (..), label)
import           Pos.Binary.Crypto               ()
import           Pos.Binary.Ssc.GodTossing.Core  ()
import           Pos.Binary.Ssc.GodTossing.Types ()
import           Pos.Ssc.GodTossing.Types.Types  (GtSecretStorage (..))


----------------------------------------------------------------------------
-- SecretStorage Type
----------------------------------------------------------------------------
instance Bi GtSecretStorage where
    put (GtSecretStorage c o e) = put c >> put o >> put e
    get = label "GtSecretStorage" $
        GtSecretStorage <$> get <*> get <*> get
