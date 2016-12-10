{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.DHT.Class.BiP
       (
         BiP(..)
       , plainBiP
       ) where

import           Control.Monad.Catch               (MonadThrow (..))
import           Control.Monad.Fail                (fail)
import           Control.TimeWarp.Rpc.Message      (ContentData (..),
                                                    HeaderNContentData (..),
                                                    HeaderNRawData (..), Message (..),
                                                    MessageName, NameData (..),
                                                    Packable (..), PackingType (..),
                                                    RawData (..), Unpackable (..),
                                                    messageName')
import           Data.Binary.Get                   (Get, isEmpty, label, runGetOrFail)
import           Data.Binary.Put                   (runPut)
import qualified Data.ByteString.Lazy              as BL
import           Data.Conduit                      ((=$=))
import qualified Data.Conduit.List                 as CL
import           Data.Conduit.Serialization.Binary (ParseError (..), conduitGet,
                                                    conduitPut)
import           Universum                         hiding (yield)

import           Pos.Binary.Class                  (Bi (..))

----------------------------------------------------------------------------
-- THIS MODULE IS HARDLY COPY-PASTED FROM TimeWarp.Rpc.Message
-- Something should be done in terms of refactoring.
----------------------------------------------------------------------------

data BiP header = BiP

runGetOrThrow :: MonadThrow m => Get a -> BL.ByteString -> m a
runGetOrThrow p s =
    either (\(bs, off, err) -> throwM $ ParseError (BL.toStrict bs) off err)
           (\(_, _, a) -> return a)
        $ runGetOrFail p s


plainBiP :: BiP ()
plainBiP = BiP

instance Bi h => PackingType (BiP h) where
    type IntermediateForm (BiP h) = HeaderNRawData h
    unpackMsg _ = conduitGet $ HeaderNRawData <$> get <*> (RawData <$> get)

instance (Bi h, Bi r, Message r)
       => Packable (BiP h) (HeaderNContentData h r) where
    packMsg p = CL.map packToRaw =$= packMsg p
      where
        packToRaw (HeaderNContentData h r) =
            HeaderNRawData h . RawData . BL.toStrict . runPut $ do
                put $ messageName' r
                put r

instance Bi h
      => Packable (BiP h) (HeaderNRawData h) where
    packMsg _ = CL.map doPut =$= conduitPut
      where
        doPut (HeaderNRawData h (RawData r)) = put h >> put r


instance Bi h
      => Unpackable (BiP h) (HeaderNRawData h) where
    extractMsgPart _ = return

instance Bi h
      => Unpackable (BiP h) NameData where
    extractMsgPart _ (HeaderNRawData _ (RawData raw)) =
        let labelName = "(in parseNameData)"
        in runGetOrThrow (NameData <$> label labelName get) $ BL.fromStrict raw

instance (Bi h, Bi r)
      => Unpackable (BiP h) (ContentData r) where
    extractMsgPart _ (HeaderNRawData _ (RawData raw)) =
        runGetOrThrow parser $ BL.fromStrict raw
      where
        parser = checkAllConsumed $ label labelName $
            (get :: Get MessageName) *> (ContentData <$> get)
        checkAllConsumed p = p <* unlessM isEmpty (fail "unconsumed input")
        labelName = "(in parseNameNContentData)"
