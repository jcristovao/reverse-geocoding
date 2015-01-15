{-# LANGUAGE OverloadedStrings #-}
-- | Here we define providers for the Reverse Geocoding feature,
-- namely how to format the url, the key under which the results are returned,
-- and how to parse the retrived data.
--
-- Currently only Open Stree Map is provided, feel free to add others.
--
module Data.Geolocation.Reverse.Providers
  ( ReverseGeoJsonKey
  , ReverseGeoParser
  , ReverseGeoProvider

  , openStreetMap

  ) where

import Control.Applicative

import Data.Monoid
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Data.Geolocation.Reverse.Types

type ReverseGeoJsonKey = T.Text
type ReverseGeoUrl = Latitude -> Longitude -> Maybe String
type ReverseGeoParser  = Object -> Parser ParsedLocationInfo
type ReverseGeoProvider = (ReverseGeoJsonKey, ReverseGeoUrl, ReverseGeoParser)


openStreetMap :: ReverseGeoProvider
openStreetMap = ("address", openStreetMapUrl, openStreetMapParser)

openStreetMapUrl :: Latitude -> Longitude -> Maybe String
openStreetMapUrl (Latitude mlat) (Longitude mlon) = do
  lat <- mlat
  lon <- mlon
  return $  "http://nominatim.openstreetmap.org/reverse"
         <> "?format=json"
         <> "&zoom=18"
         <> "&lat=" <> show lat
         <> "&lon=" <> show lon


openStreetMapParser o =
  ParsedLocationInfo <$> o .:  "country_code"
                     <*> o .:  "city"
                     <*> o .:? "suburb"
                     <*> ((o .:? "road") <|> (o .:? "street"))
                     <*> o .:? "postcode"
