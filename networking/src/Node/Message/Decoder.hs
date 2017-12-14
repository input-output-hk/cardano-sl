{-# LANGUAGE RankNTypes #-}

module Node.Message.Decoder
    ( Decoder (..)
    , DecoderStep (..)
    , ByteOffset
    , continueDecoding
    , hoistDecoder
    , hoistDecoderStep
    , pureDone
    , pureFail
    , purePartial
    ) where

import qualified Data.ByteString as BS
import           Data.Int (Int64)
import qualified Data.Text as T

type ByteOffset = Int64

data DecoderStep m t =
      Done !BS.ByteString !ByteOffset !t
    | Fail !BS.ByteString !ByteOffset !T.Text
    | Partial (Maybe BS.ByteString -> Decoder m t)

newtype Decoder m t = Decoder {
      runDecoder :: m (DecoderStep m t)
    }

hoistDecoder
    :: ( Functor n )
    => (forall a . m a -> n a)
    -> Decoder m t
    -> Decoder n t
hoistDecoder nat (Decoder m) = Decoder (hoistDecoderStep nat <$> nat m)

hoistDecoderStep
    :: ( Functor n )
    => (forall a . m a -> n a)
    -> DecoderStep m t
    -> DecoderStep n t
hoistDecoderStep nat step = case step of
    Done trailing offset t   -> Done trailing offset t
    Fail trailing offset err -> Fail trailing offset err
    Partial k                -> Partial $ hoistDecoder nat . k

-- | Feed input through a decoder.
--
continueDecoding
    :: ( Monad m )
    => DecoderStep m t
    -> BS.ByteString
    -> m (DecoderStep m t)
continueDecoding decoderStep bs = case decoderStep of
    Done trailing offset t   -> pure $ Done (BS.append trailing bs) offset t
    Fail trailing offset err -> pure $ Fail (BS.append trailing bs) offset err
    Partial k                -> runDecoder (k (Just bs))


pureDone :: Monad m => BS.ByteString -> ByteOffset -> t -> Decoder m t
pureDone trailing offset t = Decoder . return $ Done trailing offset t

pureFail :: Monad m => BS.ByteString -> ByteOffset -> T.Text -> Decoder m t
pureFail trailing offset err = Decoder . return $ Fail trailing offset err

purePartial :: Monad m => (Maybe BS.ByteString -> Decoder m t) -> Decoder m t
purePartial k = Decoder . return $ Partial k
