{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Wrappers to control generic 'ToJSON' and 'FromJSON' derivation with deriving-via.
--   See the test for example definitions and their encoding.
module AesonVia
  ( AesonRecord (..)
  , AesonNewtype (..)
  , AesonTag (..)
  , HasJSONOptions (..)
  , HasTagPrefix (..)
  )
where

import Control.Newtype.Generics (Newtype, O, pack, unpack)
import Data.Aeson
  ( FromJSON (..)
  , GFromJSON
  , GToEncoding
  , GToJSON
  , Options (..)
  , ToJSON (..)
  , Zero
  , defaultOptions
  , genericParseJSON
  , genericToEncoding
  , genericToJSON
  )
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic, Rep)
import Prelude

-- Options

recordOptions :: Options
recordOptions = (aesonPrefix snakeCase) {omitNothingFields = True}

tagOptions :: Text -> Options
tagOptions prefix =
  let prefixLen = Text.length prefix
  in  defaultOptions
        { allNullaryToStringTag = True
        , constructorTagModifier = snakeCase . drop prefixLen
        }

newtypeOptions :: Options
newtypeOptions =
  defaultOptions
    { unwrapUnaryRecords = True
    }

-- Has classes

-- | Mostly an internal class directing constructor/field conversion.
class HasJSONOptions a where
  getJSONOptions :: Proxy a -> Options

-- | Used with 'AesonTag' to define a prefix to be removed from a 'Bounded' 'Enum'.
-- For example, `data Foo = FooBar | FooBaz` would use the prefix `Foo` to yield converted string
-- values `bar` and `baz`.
class HasTagPrefix a where
  getTagPrefix :: Proxy a -> Text

-- Wrappers

-- | Generic deriving ToJSON/FromJSON via this uses 'HasTagPrefix' to turn 'Bounded' 'Enum' datatypes into enumerated strings.
newtype AesonTag a = AesonTag {unAesonTag :: a}

instance (HasTagPrefix a) => HasJSONOptions (AesonTag a) where
  getJSONOptions _ = tagOptions (getTagPrefix (Proxy :: Proxy a))

instance (HasJSONOptions (AesonTag a), Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (AesonTag a) where
  toJSON = genericToJSON (getJSONOptions (Proxy :: Proxy (AesonTag a))) . unAesonTag
  toEncoding = genericToEncoding (getJSONOptions (Proxy :: Proxy (AesonTag a))) . unAesonTag

instance (HasJSONOptions (AesonTag a), Generic a, GFromJSON Zero (Rep a)) => FromJSON (AesonTag a) where
  parseJSON = (AesonTag <$>) . genericParseJSON (getJSONOptions (Proxy :: Proxy (AesonTag a)))

-- | Generic deriving ToJSON/FromJSON via this removes the common field name prefix in the encoding.
newtype AesonRecord a = AesonRecord {unAesonRecord :: a}

instance HasJSONOptions (AesonRecord a) where
  getJSONOptions _ = recordOptions

instance (HasJSONOptions (AesonRecord a), Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (AesonRecord a) where
  toJSON = genericToJSON (getJSONOptions (Proxy :: Proxy (AesonRecord a))) . unAesonRecord
  toEncoding = genericToEncoding (getJSONOptions (Proxy :: Proxy (AesonRecord a))) . unAesonRecord

instance (HasJSONOptions (AesonRecord a), Generic a, GFromJSON Zero (Rep a)) => FromJSON (AesonRecord a) where
  parseJSON = (AesonRecord <$>) . genericParseJSON (getJSONOptions (Proxy :: Proxy (AesonRecord a)))

-- | Generic deriving ToJSON/FromJSON via this yields an encoding equivalent to the wrapped type.
newtype AesonNewtype n o = AesonNewtype {unAesonNewtype :: n}

instance HasJSONOptions (AesonNewtype n o) where
  getJSONOptions _ = newtypeOptions

instance (Newtype n, o ~ O n, ToJSON o) => ToJSON (AesonNewtype n o) where
  toJSON = toJSON . unpack . unAesonNewtype
  toEncoding = toEncoding . unpack . unAesonNewtype

instance (Newtype n, o ~ O n, FromJSON o) => FromJSON (AesonNewtype n o) where
  parseJSON = ((AesonNewtype . pack) <$>) . parseJSON
