{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Richmen part of LRC DB.

module Pos.Lrc.DB.RichmenBase
       (
         -- * Generalization
         RichmenComponent (..)

         -- * Getters
       , getRichmen
       , getRichmenP

       -- * Operations
       , putRichmen
       , putRichmenP
       ) where

import           Universum

import           Pos.Binary.Class  (encodeStrict)
import           Pos.Binary.Core   ()
import           Pos.Core.Types    (EpochIndex)
import           Pos.DB.Class      (MonadDB)
import           Pos.Lrc.Class     (RichmenComponent (..))
import           Pos.Lrc.DB.Common (getBi, putBi)
import           Pos.Lrc.Types     (FullRichmenData)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getRichmen
    :: forall c m.
       (RichmenComponent c, MonadDB m)
    => EpochIndex -> m (Maybe (RichmenData c))
getRichmen = getBi . richmenKey @c

getRichmenP
    :: forall c m.
       (RichmenComponent c, MonadDB m)
    => Proxy c -> EpochIndex -> m (Maybe (RichmenData c))
getRichmenP Proxy = getRichmen @c

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

putRichmen
    :: forall c m.
       (RichmenComponent c, MonadDB m)
    => EpochIndex -> FullRichmenData -> m ()
putRichmen e = putBi (richmenKey @c e) . (rcToData @c)

putRichmenP
    :: forall c m.
       (RichmenComponent c, MonadDB m)
    => Proxy c -> EpochIndex -> FullRichmenData -> m ()
putRichmenP _ = putRichmen @c

richmenKey
    :: forall c.
       RichmenComponent c
    => EpochIndex -> ByteString
richmenKey = richmenKeyP (Proxy @c)

richmenKeyP
    :: forall c.
       RichmenComponent c
    => Proxy c -> EpochIndex -> ByteString
richmenKeyP proxy e = mconcat ["r/", rcTag proxy, "/", encodeStrict e]
