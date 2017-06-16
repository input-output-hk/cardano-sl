{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Util.Config
       (
       -- * Small configs
         IsConfig(..)
       , configParser
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

       -- * Multi-configs
       , ConfigSet(..)
       , consConfigSet
       , readConfigSet
       , unsafeReadConfigSet

       -- * Cardano SL config
       , cslConfig
       , parseFromCslConfig
       -- ** Internal functions
       , cslConfigFilePath
       , getCslConfig
       ) where

import           Control.Lens               (Getting, _Left)
import qualified Data.Aeson                 as Y (withObject)
import           Data.Tagged                (Tagged, untag)
import           Data.Yaml                  (FromJSON)
import qualified Data.Yaml                  as Y
import           Universum

#if EMBED_CONFIG
import qualified Language.Haskell.TH.Syntax as TH
#else
import           System.IO.Unsafe           (unsafePerformIO)
#endif

import           Pos.Util.Config.Get        (getCslConfig)
import           Pos.Util.Config.Path       (cslConfigFilePath)
import           Pos.Util.HVect             (HVect)
import qualified Pos.Util.HVect             as HVect

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
configParser :: forall c. IsConfig c => Y.Value -> Y.Parser c
configParser = Y.withObject "config" $ \obj ->
    case untag @c configPrefix of
        Nothing  -> Y.parseJSON (Y.Object obj)
        Just key -> Y.parseJSON =<< obj Y..: key

-- | Parse a config from a bigger YAML file.
readConfig :: IsConfig c => FilePath -> IO (Either String c)
readConfig fp =
    Y.decodeFileEither fp <&> \case
        Right y -> Y.parseEither configParser y
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
Using 'getFullConfig' is not very convenient because usually we don't need
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

-- | Get a config.
getConfig :: HasConfig a m => m a
getConfig = extractConfig <$> getFullConfig
{-# INLINE getConfig #-}

-- | Get some field from a config.
askConfig :: HasConfig a m => (a -> x) -> m x
askConfig f = f <$> getConfig
{-# INLINE askConfig #-}

-- | Get some lens from a config.
viewConfig :: HasConfig a m => Getting x a x -> m x
viewConfig f = view f <$> getConfig
{-# INLINE viewConfig #-}

----------------------------------------------------------------------------
-- ConfigSet
----------------------------------------------------------------------------

{- | @ConfigSet '[A, B, C]@ is a collection of configs from which you can
extract @A@, @B@ or @C@. You're expected to use it instead of creating your
own @Config@ type with subconfigs.

A 'ConfigSet' is intended to have only one config of each given type, but
this condition is not checked.
-}
newtype ConfigSet (xs :: [*]) = ConfigSet (HVect xs)

-- | Add a config to a 'ConfigSet'.
consConfigSet :: x -> ConfigSet xs -> ConfigSet (x ': xs)
consConfigSet x (ConfigSet v) = ConfigSet (HVect.cons x v)

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

instance HVect.Contains x xs => ExtractConfig x (ConfigSet xs) where
    extractConfig (ConfigSet v) = HVect.extract v

-- If all types in a 'ConfigSet' are configs, we can parse a whole JSON\/YAML
-- config into a 'ConfigSet' by parsing each config separately.
instance (IsConfig x, FromJSON (ConfigSet xs)) =>
         FromJSON (ConfigSet (x ': xs)) where
    parseJSON val = do
        x <- configParser val
        xs <- Y.parseJSON @(ConfigSet xs) val
        return (consConfigSet x xs)

instance FromJSON (ConfigSet '[]) where
    parseJSON = \_ -> return (ConfigSet HVect.empty)

----------------------------------------------------------------------------
-- Cardano SL config
----------------------------------------------------------------------------

-- | The config as a YAML value. In development mode it's read at the start
-- of the program, in production mode it's embedded into the file.
--
-- The config (constants.yaml) is actually three configs in one: depending on
-- the value of @CONFIG@ (a variable passed via CPP), 'cslConfig' will either
-- be a @dev@, @prod@ or @wallet@ config.
--
-- TODO: allow overriding config values via an env var?
cslConfig :: Y.Value
#ifdef EMBED_CONFIG
cslConfig = $(do
    TH.qAddDependentFile cslConfigFilePath
    either fail TH.lift =<< TH.runIO getCslConfig
  )
#else
cslConfig = unsafePerformIO $ either fail pure =<< getCslConfig
{-# NOINLINE cslConfig #-}
#endif

-- | Read a value from 'cslConfig'.
--
-- Usually you would pass 'configParser' here if you need to extract a single
-- config from a hierarchical config, or 'parseJSON' if you need to read a
-- 'ConfigSet'.
parseFromCslConfig :: (Y.Value -> Y.Parser a) -> Either String a
parseFromCslConfig p = Y.parseEither p cslConfig
