{-# LANGUAGE TypeFamilies #-}

-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.Communication.BiP
       ( BiP(..)
       ) where

import           Universum

import           Control.Monad.Catch              (onException)
import           Control.Monad.Trans.Either       (EitherT, left)
import qualified Control.Monad.Trans.State.Strict as St
import           Data.Binary.Get                  (runGetIncremental)
import           Data.Binary.Put                  (execPut)
import qualified Data.ByteString.Builder.Extra    as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Store.Core                  (Peek, decodeIOWithFromPtr)
import           Data.Store.Streaming             (FillByteBuffer, PeekMessage)
import           Node.Message                     (Packable (..), UnpackMsg,
                                                   Unpackable (..), UnpackableCtx (..),
                                                   needMoreInput)
import           System.IO.ByteBuffer             (ByteBuffer)
import qualified System.IO.ByteBuffer             as BB

import           Pos.Binary.Class                 (Bi (..))

data BiP = BiP

instance Bi r => Packable BiP r where
    packMsg _ m = undefined

instance Bi r => Unpackable BiP r where
    unpackMsg _ = undefined

peekSized :: (MonadCatch m, MonadIO m) => Int -> Peek a -> UnpackMsg ByteBuffer m a
peekSized n peek = go
  where
    err msg = lift $ left msg
    go = lift (lift St.get) >>= go'
    go' Nothing = needMoreInput >>= \case
                    Right bb -> putGo bb `onException` BB.free bb
                    Left Nothing -> err "no more input"
                    Left (Just x) -> do
                        bb <- BB.new Nothing
                        (`onException` BB.free bb) $ do
                            BB.copyByteString bb x
                            putGo bb
      where
        putGo bb = lift (lift $ St.put $ Just bb) *> go
    go' (Just bb) = do
        mbPtr <- BB.unsafeConsume bb n
        case mbPtr of
            Left needed -> do
                inp <- needMoreInput
                case inp of
                    -- This case should happen in practice
                    Right bb' ->
                      let copy = do
                              av <- BB.availableBytes bb'
                              eBs <- BB.consume bb' av
                              case eBs of
                                  Left i   -> err "no more input"
                                  Right bs -> BB.copyByteString bb bs
                       in -- Here we use onException, though `finally` would be ideal here
                          -- This is because FT has no instance for MonadMask
                          (copy `onException` BB.free bb') <* BB.free bb'
                    Left Nothing -> err "no more input"
                    Left (Just x) -> BB.copyByteString bb x
                go
            Right ptr -> do
                res <- liftIO $ decodeIOWithFromPtr peek ptr n
                ifM (BB.isEmpty bb)
                    (BB.free bb *> lift (lift $ St.put Nothing) $> res)
                    (pure res)



instance UnpackableCtx BiP where
    type (Unconsumed BiP) = ByteBuffer
    type (UnpackMonad BiP) = IO

