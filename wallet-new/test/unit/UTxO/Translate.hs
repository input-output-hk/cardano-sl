{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UTxO.Translate (
    -- * Monadic context for the translation from the DSL to Cardano
    TranslateT
  , Translate
  , runTranslateT
  , runTranslate
  , runTranslateNoErrors
  , withConfig
  , mapTranslateErrors
  , catchTranslateErrors
    -- * Interface to the verifier
  , verify
  , verifyBlocksPrefix
    -- * Convenience re-exports
  , MonadError(..)
  , MonadGState(..)
  ) where

import Universum
import Control.Exception (throw)
import Control.Monad.Except
import Data.Constraint (Dict(..))

import Pos.Block.Error
import Pos.Block.Types
import Pos.Core
import Pos.DB.Class (MonadGState(..))
import Pos.Txp.Toil
import Pos.Update
import Pos.Util.Chrono

import UTxO.Context
import UTxO.Verify (Verify)
import qualified UTxO.Verify as Verify

{-------------------------------------------------------------------------------
  Testing infrastructure from cardano-sl-core

  The genesis block comes from defaultTestConf, which in turn uses
  configuration.yaml. It is specified by a 'GenesisSpec'.
-------------------------------------------------------------------------------}

import Test.Pos.Configuration (
    withDefConfiguration
  , withDefUpdateConfiguration
  )

{-------------------------------------------------------------------------------
  Translation monad

  The translation provides access to the translation context as well as some
  dictionaries so that we can lift Cardano operations to the 'Translate' monad.
  (Eventually we may wish to do this differently.)
-------------------------------------------------------------------------------}

data TranslateEnv = TranslateEnv {
      teContext :: TransCtxt
    , teConfig  :: Dict HasConfiguration
    , teUpdate  :: Dict HasUpdateConfiguration
    }

newtype TranslateT e m a = TranslateT {
      unTranslateT :: ExceptT e (ReaderT TranslateEnv m) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError e
           )

instance MonadTrans (TranslateT e) where
  lift = TranslateT . lift . lift

type Translate e = TranslateT e Identity

instance Monad m => MonadReader TransCtxt (TranslateT e m) where
  ask     = TranslateT $ asks teContext
  local f = TranslateT . local f' . unTranslateT
    where
      f' env = env { teContext = f (teContext env) }

-- | Right now this always returns the genesis policy
instance Monad m => MonadGState (TranslateT e m) where
  gsAdoptedBVData = withConfig $ return genesisBlockVersionData

-- | Run translation
--
-- NOTE: This uses the default test configuration, and throws any errors as
-- pure exceptions.
runTranslateT :: Monad m => Exception e => TranslateT e m a -> m a
runTranslateT (TranslateT ta) =
    withDefConfiguration $
    withDefUpdateConfiguration $
      let env :: TranslateEnv
          env = TranslateEnv {
                    teContext = initContext initCardanoContext
                  , teConfig  = Dict
                  , teUpdate  = Dict
                  }
      in do ma <- runReaderT (runExceptT ta) env
            case ma of
              Left  e -> throw  e
              Right a -> return a

-- | Specialization of 'runTranslateT'
runTranslate :: Exception e => Translate e a -> a
runTranslate = runIdentity . runTranslateT

-- | Specialised form of 'runTranslate' when there can be no errors
runTranslateNoErrors :: Translate Void a -> a
runTranslateNoErrors = runTranslate

-- | Lift functions that want the configuration as type class constraints
withConfig :: Monad m
           => ((HasConfiguration, HasUpdateConfiguration) => TranslateT e m a)
           -> TranslateT e m a
withConfig f = do
    Dict <- TranslateT $ asks teConfig
    Dict <- TranslateT $ asks teUpdate
    f

-- | Map errors
mapTranslateErrors :: Functor m
                   => (e -> e') -> TranslateT e m a -> TranslateT e' m a
mapTranslateErrors f (TranslateT ma) = TranslateT $ withExceptT f ma

-- | Catch and return errors
catchTranslateErrors :: Functor m
                     => TranslateT e m a -> TranslateT e' m (Either e a)
catchTranslateErrors (TranslateT (ExceptT (ReaderT ma))) =
    TranslateT $ ExceptT $ ReaderT $ \env -> fmap Right (ma env)

{-------------------------------------------------------------------------------
  Interface to the verifier
-------------------------------------------------------------------------------}

-- | Run the verifier
verify :: Monad m
       => (HasConfiguration => Verify e a)
       -> TranslateT e' m (Either e (a, Utxo))
verify ma = withConfig $ do
    utxo <- asks (ccUtxo . tcCardano)
    return $ Verify.verify utxo ma

-- | Wrapper around 'UTxO.Verify.verifyBlocksPrefix'
verifyBlocksPrefix
  :: Monad m
  => OldestFirst NE Block
  -> TranslateT e' m (Either VerifyBlocksException (OldestFirst NE Undo, Utxo))
verifyBlocksPrefix blocks = do
    CardanoContext{..} <- asks tcCardano
    let tip         = ccHash0
        currentSlot = Nothing
    verify $ Verify.verifyBlocksPrefix
        tip
        currentSlot
        ccLeaders        -- TODO: May not be necessary to pass this if we start from genesis
        (OldestFirst []) -- TODO: LastBlkSlots. Unsure about the required value or its effect
        blocks
