import Data.Geolocation.Reverse
import Data.Geolocation.Reverse.Types

main = do
  putStrLn "Network test stub"
  -- all we're going to do is get a location and
  -- check we don't crash.
  l <- getLocationInfoDef (Latitude (Just 5)) (Longitude (Just 5))
  print l
