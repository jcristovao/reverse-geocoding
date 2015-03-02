{-# LANGUAGE OverloadedStrings #-}
-- | This very simple module returns a Maybe Location info for a set
-- of coordinates.
--
-- Right now only one provider is given (OpenStreeMap), please feel
-- free to add more.
--
-- We use this for a very coarse geographic categorization, so
-- the following equivalences hold:
--
-- City  : city   \<|> village \<|> town     \<|> hamlet \<|> county
--
-- Suburb: suburb \<|> hamlet  \<|> postcode \<|> town
--
-- Street: road   \<|> street
--
-- All contribuitions are welcomed!
module Data.Geolocation.Reverse where

import Control.Lens
import Network.Wreq
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens (key)

import Data.Geolocation.Reverse.Types
import Data.Geolocation.Reverse.Providers

-- | Default reverse geo provider
defaultReverseGeoProvider :: ReverseGeoProvider
defaultReverseGeoProvider = openStreetMap

-- | Given a Latitude and Longitude, return a Location Info.
getLocationInfo
  :: Latitude
  -> Longitude
  -> ReverseGeoProvider
  -> IO (Maybe ParsedLocationInfo)
getLocationInfo lat lon (jsonkey,url,parser) =
  case url lat lon  of
    Nothing -> return Nothing
    Just ga -> do
      r   <- get ga
      --    addr :: Maybe Value
      let addr = r ^? responseBody . key jsonkey
      case addr of
        Just (Object o) -> return . flip parseMaybe o . return
            $ parser o
        _ -> return Nothing

-- | Get location info from default provider (right now Open Stree Map)
getLocationInfoDef :: Latitude -> Longitude -> IO (Maybe ParsedLocationInfo)
getLocationInfoDef lat lon = getLocationInfo lat lon defaultReverseGeoProvider
