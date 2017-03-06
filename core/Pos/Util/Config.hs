{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Util.Config
       (
       -- * Small configs
         IsConfig(..)
       , parseConfig
       , readConfig
       , unsafeReadConfig

       -- * Bigger configs
       , ExtractConfig(..)
       , MonadConfig(..)
       , HasConfig
       , HasConfigs
       , getConfig
       , askConfig
       , viewConfig

       -- * 'ConfigT'
       , ConfigT(..)
       , runConfigT
       , usingConfigT

       -- * Multi-configs
       , ConfigSet(..)
       , consConfigSet
       , readConfigSet
       , unsafeReadConfigSet

       -- * Cardano SL config
       , cslConfigFilePath
       ) where

import           Control.Lens     (Getter, _Left)
import           System.Directory (doesFileExist)
import           System.FilePath  ((</>))
import           Data.Yaml        (FromJSON)
import qualified Data.Yaml        as Y
import qualified Data.Aeson       as Y (withObject)
import           Data.Tagged      (untag, Tagged)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           GHC.TypeLits     (type (+))
import           Universum
import           Unsafe.Coerce    (unsafeCoerce)

import           Paths_cardano_sl_core (getDataFileName)

----------------------------------------------------------------------------
-- Small configs
----------------------------------------------------------------------------

{- |
A config is something that can be converted from JSON and potentially has a
prefix. The prefix will be used when parsing a bigger config which @config@
is a part of. For instance, if @FooCfg@ has fields @a@ and @b@, @BarCfg@ has
fields @x@ and @y@, and 'configPrefix' of @FooCfg@ is @Nothing@, the big
config might look like this:

@
a: ...
b: ...
x: ...
y: ...
@

However, if the prefix is @Just "foo"@, the config will be like this:

@
foo:
  a: ...
  b: ...
x: ...
y: ...
@
-}
class FromJSON config => IsConfig config where
    configPrefix :: Tagged config (Maybe Text)

-- | Parse a config from a bigger JSON\/YAML value using the scheme above.
parseConfig :: forall c. IsConfig c => Y.Value -> Y.Parser c
parseConfig = Y.withObject "config" $ \obj ->
    case untag @c configPrefix of
        Nothing  -> Y.parseJSON (Y.Object obj)
        Just key -> Y.parseJSON =<< obj Y..: key

-- | Parse a config from a bigger YAML file.
readConfig :: IsConfig c => FilePath -> IO (Either String c)
readConfig fp =
    Y.decodeFileEither fp <&> \case
        Right y -> Y.parseEither parseConfig y
        Left x -> Left (Y.prettyPrintParseException x)

-- | Like 'readConfig', but throws an exception at runtime instead of
-- returning an 'Either'.
unsafeReadConfig :: IsConfig c => FilePath -> IO c
unsafeReadConfig fp =
    readConfig fp >>= \case
        Right x  -> return x
        Left err -> fail $ "Error while parsing " ++ fp ++ ":\n" ++ err

----------------------------------------------------------------------------
-- Classes etc
----------------------------------------------------------------------------

{- |
Big configs can be split into smaller configs with 'extractConfig'. In the
simplest scenario, you would define @FooCfg@ and @BarCfg@ in “foo” and “bar”
components of your program, and then @data Config = Config FooCfg BarCfg@ on
the top level. Then you'd define two instances:

@
instance ExtractConfig FooCfg Config where
    extractConfig (Config foo _) = foo
instance ExtractConfig BarCfg Config where
    extractConfig (Config _ bar) = bar
@

Though it's expected that you would just use 'ConfigSet' instead of defining
a “container” config type.
-}
class ExtractConfig a s where
    extractConfig :: s -> a

{- |
The config will usually be stored in your monad stack. To access it, we
define an MTL-style class called 'MonadConfig' with method 'getFullConfig',
but instead of using a multiparameter type class (like 'MonadReader') we
define an associated type called 'ConfigType'.
-}
class Monad m => MonadConfig m where
    type ConfigType m
    getFullConfig :: m (ConfigType m)

{- |
Using 'getFullConfig' if not very convenient because usually we don't need
the full config. Thus we define a type synonym 'HasConfig' which combines
'MonadConfig' and 'ExtractConfig' – in particular, @HasConfig A m@ means that
inside @m@ you can access config of type @A@.
-}
type HasConfig a m = (MonadConfig m, ExtractConfig a (ConfigType m))

{- |
Declaring that a function needs access to several configs can be done with
the 'HasConfigs' type family:

@
HasConfigs [A,B,C] m === (HasConfig A m, HasConfig B m, HasConfig C m)
@
-}
type family HasConfigs (xs :: [*]) m :: Constraint where
    HasConfigs    '[]    _ = ()
    HasConfigs (x ': xs) m = (HasConfig x m, HasConfigs xs m)

-- | Get the config.
getConfig :: HasConfig a m => m a
getConfig = extractConfig <$> getFullConfig
{-# INLINE getConfig #-}

-- | Get some field from the config.
askConfig :: HasConfig a m => (a -> x) -> m x
askConfig f = f <$> getConfig
{-# INLINE askConfig #-}

-- | Get some lens from the config.
viewConfig :: HasConfig a m => Getter a x -> m x
viewConfig f = view f <$> getConfig
{-# INLINE viewConfig #-}

----------------------------------------------------------------------------
-- ConfigT
----------------------------------------------------------------------------

{- |
'ConfigT' is a specific transformer that can be used for 'MonadConfig'. It's
isomorphic to 'ReaderT'.
-}
newtype ConfigT config m a = ConfigT {getConfigT :: ReaderT config m a}
    deriving (Functor, Applicative, Monad)

runConfigT :: ConfigT config m a -> config -> m a
runConfigT act cfg = runReaderT (getConfigT act) cfg

usingConfigT :: config -> ConfigT config m a -> m a
usingConfigT cfg act = runReaderT (getConfigT act) cfg

----------------------------------------------------------------------------
-- ConfigSet
----------------------------------------------------------------------------

{- |
@ConfigSet '[A, B, C]@ is a collection of configs from which you can extract
@A@, @B@ or @C@. In other words, it's a heterogenous list (but fast). You're
expected to use it instead of creating your own @Config@ type with
subconfigs.

A 'ConfigSet' is intended to have only one config of each given type, but this
condition is not checked.
-}
newtype ConfigSet (xs :: [*]) =
    -- the underlying type is a vector of untyped values
    ConfigSet (Vector Any)

-- | Add a config to a 'ConfigSet'.
consConfigSet :: x -> ConfigSet xs -> ConfigSet (x ': xs)
consConfigSet x (ConfigSet xs) = ConfigSet (V.cons (unsafeCoerce x) xs)

-- | Parse a 'ConfigSet' from a YAML file.
readConfigSet
    :: FromJSON (ConfigSet xs)
    => FilePath -> IO (Either String (ConfigSet xs))
readConfigSet =
    fmap (over _Left Y.prettyPrintParseException) . Y.decodeFileEither

-- | Like 'readConfigSet', but throws an exception at runtime.
unsafeReadConfigSet
    :: FromJSON (ConfigSet xs)
    => FilePath -> IO (ConfigSet xs)
unsafeReadConfigSet fp =
    readConfigSet fp >>= \case
        Right x  -> return x
        Left err -> fail $ "Error while parsing " ++ fp ++ ":\n" ++ err

----------------------------------------------------------------------------
-- ConfigSet magic
----------------------------------------------------------------------------

-- We can use 'Index' to find the position of a type in a list of types. The
-- result is a 'Nat', i.e. a type-level number.
type family Index (x :: *) (xs :: [*]) :: Nat where
    Index x (x ': _)  = 0
    Index x (_ ': xs) = 1 + Index x xs

-- If we know index of a type in a list of types stored in a 'ConfigSet', we
-- can extract it from the underlying vector and convert it to the right type
-- with 'unsafeCoerce'.
instance KnownNat (Index x xs) => ExtractConfig x (ConfigSet xs) where
    extractConfig (ConfigSet v) =
        let i = natVal (Proxy @(Index x xs))
        in  unsafeCoerce (v V.! fromIntegral i)

-- If all types in a 'ConfigSet' are configs, we can parse a whole JSON\/YAML
-- config into a 'ConfigSet' by parsing each config separately.
instance (IsConfig x, FromJSON (ConfigSet xs)) =>
         FromJSON (ConfigSet (x ': xs)) where
    parseJSON val = do
        x <- parseConfig val
        xs <- Y.parseJSON @(ConfigSet xs) val
        return (consConfigSet x xs)

instance FromJSON (ConfigSet '[]) where
    parseJSON = \_ -> return (ConfigSet mempty)

----------------------------------------------------------------------------
-- Cardano SL config
----------------------------------------------------------------------------

-- | Path to config that should be used by all parts of Cardano SL (depend on
-- whether we're in DEV_MODE or not).
cslConfigFilePath :: IO FilePath
cslConfigFilePath = do
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
