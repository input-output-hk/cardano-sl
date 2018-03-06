{-# LANGUAGE RankNTypes #-}

-- | Conduit utilities.
module Pos.Binary.Conduit
       ( awaitBi
       , awaitCbor
       ) where

import           Universum

import qualified Codec.CBOR.Decoding as Cbor hiding (DecodeAction (..))
import qualified Codec.CBOR.Read as Cbor
import           Conduit (ConduitT)
import qualified Conduit as C
import           Control.Monad.ST (RealWorld, ST)
import           Data.Conduit (transPipe)
import           Data.NonNull (toNullable)

import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi

-- | Decode one 'Bi'-serialized value from the input.
--
-- Returns a 'Nothing' if there's no input left.
awaitBi
    :: forall o m a . (MonadIO m, Bi a)
    => ConduitT ByteString o m (Maybe (Either Cbor.DeserialiseFailure a))
awaitBi = awaitCbor Bi.decode

-- | Like 'awaitBi', but allows supplying a custom decoder.
--
-- TODO: it should be possible to avoid 'MonadIO' here, but I don't know
-- how. (On the other hand, see <https://redd.it/1vcvxe> – maybe it's
-- impossible after all.)
awaitCbor
    :: forall o m a . (MonadIO m)
    => Cbor.Decoder RealWorld a
    -> ConduitT ByteString o m (Maybe (Either Cbor.DeserialiseFailure a))
awaitCbor decoder = transPipe stToIO $ do
    mbChunk <- awaitNonNull
    case mbChunk of
        Nothing    -> pure Nothing
        Just chunk -> do
            C.leftover chunk
            initResult <- lift (Cbor.deserialiseIncremental decoder)
            Just <$> go False initResult
  where
    go :: forall o1 a1
        . Bool                    -- ^ End of input reached?
       -> Bi.IDecode RealWorld a1  -- ^ Result of decoding so far
       -> ConduitT ByteString o1 (ST RealWorld) (Either Cbor.DeserialiseFailure a1)
    go eof = \case
        Cbor.Fail rest _ failure -> do
            C.leftover rest
            pure (Left failure)
        Cbor.Partial f | not eof -> do
            mbChunk <- awaitNonNull
            go (isNothing mbChunk) =<< lift (f mbChunk)
        Cbor.Partial _ ->    -- impossible, I think
            let err = "CBOR decoder returned 'Partial' after end of input"
            in  pure (Left (Cbor.DeserialiseFailure (-1) err))
        Cbor.Done rest _ x -> do
            C.leftover rest
            pure (Right x)

    -- Like C.awaitNonNull but without the NonNull wrapper nonsense
    awaitNonNull = fmap toNullable <$> C.awaitNonNull
