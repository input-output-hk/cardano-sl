{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Util.Config
       ( ConfigT(..)
       , runConfigT
       , usingConfigT

       , HasConfig
       , IsConfig(..)
       , getConfig

       , ConfigSet(..)
       , consConfigSet
       , readConfigSet

       , readConfig
       , unsafeReadConfig
       , configFilePath
       ) where

import           System.Directory (doesFileExist)
import           System.FilePath  ((</>))
import           Formatting       ((%), build, formatToString)
import           Data.Yaml        (FromJSON)
import qualified Data.Yaml        as Y
import qualified Data.Aeson       as Y (withObject)
import           Data.Tagged      (untag, Tagged)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           GHC.TypeLits     (ErrorMessage ((:<>:)), Nat, TypeError, type (+))
import qualified GHC.TypeLits     as Lit
import           Universum
import           Unsafe.Coerce    (unsafeCoerce)

import           Paths_cardano_sl_core (getDataFileName)

type family Elem (x :: *) (xs :: [*]) :: Constraint where
    Elem x (x ': _)  = ()
    Elem x (_ ': xs) = Elem x xs
    Elem x '[]       = TypeError
        ('Lit.Text "Config " ':<>: 'Lit.ShowType x ':<>:
         'Lit.Text " has not been provided")

class Has x s where
    extract :: s -> x

instance Has a (a, b) where
    extract = fst
instance Has b (a, b) where
    extract = snd

newtype ConfigT config m a = ConfigT {getConfigT :: ReaderT config m a}
    deriving (Functor, Applicative, Monad)

runConfigT :: ConfigT config m a -> config -> m a
runConfigT act cfg = runReaderT (getConfigT act) cfg

usingConfigT :: config -> ConfigT config m a -> m a
usingConfigT cfg act = runReaderT (getConfigT act) cfg

class Monad m => MonadConfig m where
    type ConfigType m
    getFullConfig :: m (ConfigType m)

instance Monad m => MonadConfig (ConfigT config m) where
    type ConfigType (ConfigT config m) = config
    getFullConfig = ConfigT ask

getConfig
    :: forall cfg m. (MonadConfig m, IsConfig cfg, Has cfg (ConfigType m))
    => m cfg
getConfig = extract <$> getFullConfig

-- | @HasConfig [A, B, C] m@ means that inside @m@ you can access configs
-- @A@, @B@, @C@ with 'getConfig'.
type family HasConfig (xs :: [*]) m :: Constraint where
    HasConfig    '[]    _ = ()
    HasConfig (x ': xs) m = (Has x (ConfigType m), HasConfig xs m)

class FromJSON config => IsConfig config where
    configPrefix :: Tagged config (Maybe Text)

parseConfig :: forall c. IsConfig c => Y.Value -> Y.Parser c
parseConfig = Y.withObject "config" $ \obj ->
    case untag @c configPrefix of
        Nothing  -> Y.parseJSON (Y.Object obj)
        Just key -> Y.parseJSON =<< obj Y..: key

newtype ConfigSet (xs :: [*]) = ConfigSet (Vector Any)

consConfigSet :: x -> ConfigSet xs -> ConfigSet (x ': xs)
consConfigSet x (ConfigSet xs) = ConfigSet (V.cons (unsafeCoerce x) xs)

instance KnownNat (Index x xs) => Has x (ConfigSet xs) where
    extract (ConfigSet v) =
        let i = natVal (Proxy @(Index x xs))
        in  unsafeCoerce (v V.! fromIntegral i)

type family Index x xs :: Nat where
    Index x (x ': _)  = 0
    Index x (_ ': xs) = 1 + Index x xs

instance FromJSON (ConfigSet '[]) where
    parseJSON = \_ -> return (ConfigSet mempty)

instance (IsConfig x, FromJSON (ConfigSet xs)) =>
         FromJSON (ConfigSet (x ': xs)) where
    parseJSON val = do
        x <- parseConfig val
        xs <- Y.parseJSON @(ConfigSet xs) val
        return (consConfigSet x xs)

readConfigSet
    :: FromJSON (ConfigSet xs)
    => FilePath -> IO (Either Y.ParseException (ConfigSet xs))
readConfigSet = Y.decodeFileEither

readConfig :: IsConfig c => FilePath -> IO (Either String c)
readConfig fp =
    Y.decodeFileEither fp <&> \case
        Right y -> Y.parseEither parseConfig y
        Left x -> Left (show x)

unsafeReadConfig :: IsConfig c => FilePath -> IO c
unsafeReadConfig fp =
    readConfig fp >>= \case
        Right x  -> return x
        Left err -> fail $ formatToString
            ("unsafeReadConfig: couldn't read config "%build%": "%build)
            fp err

configFilePath :: IO FilePath
configFilePath = do
#if defined(DEV_MODE)
    let name = "constants-dev.yaml"
#elif defined(WITH_WALLET)
    let name = "constants-wallet-prod.yaml"
#else
    let name = "constants-prod.yaml"
#endif
    existsA <- doesFileExist (".." </> name)
    existsB <- doesFileExist name
    if | existsA   -> return (".." </> name)
       | existsB   -> return name
       | otherwise -> getDataFileName name
