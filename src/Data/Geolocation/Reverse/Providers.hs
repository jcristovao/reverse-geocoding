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

import Data.Char
import Data.Monoid
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Control.Monad (join)
import qualified Data.Text as T
import Text.Printf

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
         <> "&lat=" <> formatFixed lat
         <> "&lon=" <> formatFixed lon


getPostCodeText :: Suburb -> Maybe Suburb
getPostCodeText (Suburb txt) = let
  rest = T.dropWhile (\c -> isDigit c || isSpace c || (c == '-')) txt
  in if T.null rest then Nothing else Just (Suburb rest)

(<|?>) :: Parser (Maybe a) -> Parser (Maybe a) -> Parser (Maybe a)
pa <|?> pb = do
  a <- pa
  if isJust a then pa else pb
infixl 3 <|?>


-- Not all records have suburb defined, but some have it written in the post code
-- This function tries to extract if from the post code if available.
openStreetMapParser :: Object -> Parser ParsedLocationInfo
openStreetMapParser o =
  ParsedLocationInfo <$> o .:  "country_code"
                     <*> (    o .: "city"
                          <|> o .: "village"
                          <|> o .: "town"
                          <|> o .: "hamlet"
                          <|> o .: "county"
                          )
                     <*> (     (o .:? "suburb")
                          <|?> (o .:? "hamlet")
                          <|?> (fmap (join . fmap getPostCodeText) ( o .:? "postcode"))
                          <|?> (o .:? "town")
                          )
                     <*> (     (o .:? "road")
                          <|?> (o .:? "street")
                         )
                     <*> o .:? "postcode"

formatFixed n = printf "%.6f" n
