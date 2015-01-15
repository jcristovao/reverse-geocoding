{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Geolocation.Reverse.Types where

import Data.Aeson
import Control.Monad (mzero)
import qualified Data.Text as T
import Data.ISO3166_CountryCodes

-- | /Input types/

-- | Latitude : newtype over double
newtype Latitude  = Latitude  (Maybe Double)
  deriving (Eq,Ord,Show,Read,FromJSON,ToJSON)

-- | Longitude : newtype over double
newtype Longitude = Longitude (Maybe Double)
  deriving (Eq,Ord,Show,Read,FromJSON,ToJSON)

-- | /Output types/
newtype City    = City      T.Text deriving (Eq,Ord,Show,Read,FromJSON,ToJSON)
newtype Suburb  = Suburb    T.Text deriving (Eq,Ord,Show,Read,FromJSON,ToJSON)
newtype Street  = Street    T.Text deriving (Eq,Ord,Show,Read,FromJSON,ToJSON)
newtype Postcode= Postcode  T.Text deriving (Eq,Ord,Show,Read,FromJSON,ToJSON)

-- Booo: orphan instances
instance ToJSON CountryCode
instance FromJSON CountryCode where
  parseJSON (String t) = return . read . T.unpack . T.toUpper $ t
  parseJSON _ = mzero


-- | Parsed Location Info: Country code and city are mandatory, all other info is optional
data ParsedLocationInfo
  = ParsedLocationInfo
     { parsedCountry  :: CountryCode
     , parsedCity     :: City
     , parsedSuburb   :: Maybe Suburb
     , parsedStreet   :: Maybe Street
     , parsedPostCode :: Maybe Postcode
     } deriving (Eq,Ord,Show)
