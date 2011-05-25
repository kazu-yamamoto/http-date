module Main where

import Criterion.Main
import Data.Time
import Model
import Network.HTTP.Date
import Network.HTTP.Date

main :: IO ()
main = defaultMain [
    bgroup "format" [
         bench "formatHTTPDate" (whnf formatHTTPDate hd)
       , bench "utcToDate" (whnf utcToDate utc)
       ]
  ]
  where
    hd = defaultHTTPDate {
        hdYear   = 2000
      , hdMonth  = 2
      , hdDay    = 9
      , hdHour   = 10
      , hdMinute = 9
      , hdSecond = 20
      , hdWkday  = 6
      }
    utc = UTCTime (ModifiedJulianDay 51583) 36560