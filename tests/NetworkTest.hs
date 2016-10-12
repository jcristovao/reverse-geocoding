import Data.Geolocation.Reverse
import Data.Geolocation.Reverse.Types

main = do

  -- all we're going to do is get a location and
  -- check we don't crash.
  l <- getLocationInfoDef (Latitude (Just 5)) (Longitude (Just 5))
  print l

  -- regression test for extremely small numbers that
  -- were being rendered using invalid exponential
  -- notation.
  l2 <- getLocationInfoDef (Latitude (Just 0.0005)) (Longitude (Just 0.0005))
  print l2
