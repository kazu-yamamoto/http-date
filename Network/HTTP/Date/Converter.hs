module Network.HTTP.Date.Converter (epochTimeToHTTPDate) where

import Data.Word
import Network.HTTP.Date.Types
import System.Posix.Types

{-|
  Translating 'EpochTime' to 'HTTPDate'.
-}
epochTimeToHTTPDate :: EpochTime -> HTTPDate
epochTimeToHTTPDate x = defaultHTTPDate {
    hdYear   = y
  , hdMonth  = m
  , hdDay    = d
  , hdHour   = h
  , hdMinute = n
  , hdSecond = s
  , hdWkday  = w
  }
  where
    w64 :: Word64
    w64 = truncate . toRational $ x
    (days',secs') = w64 `divMod` 86400
    days = fromIntegral days'
    secs = fromIntegral secs'
    -- 1970/1/1 is Thu (4)
    w = (days + 3) `mod` 7 + 1
    (y,m,d) = toYYMMDD days
    (h,n,s) = toHHMMSS secs

toYYMMDD :: Int -> (Int,Int,Int)
toYYMMDD x = (yy, mm, dd)
  where
    (y,d) = x `divMod` 365
    cy = 1970 + y
    cy' = cy - 1
    leap = cy' `div` 4 - cy' `div` 100 + cy' `div` 400 - 477
    (yy,days) = adjust cy d leap
    (mm,dd) = findMonth 1 monthDays (days + 1)
    adjust ty td aj
      | td >= aj        = (ty, td - aj)
      | isLeap (ty - 1) = if td + 366 >= aj
                          then (ty - 1, td + 366 - aj)
                          else adjust (ty - 1) (td + 366) aj
      | otherwise       = if td + 365 >= aj
                          then (ty - 1, td + 365 - aj)
                          else adjust (ty - 1) (td + 365) aj
    isLeap year = year `mod` 4 == 0
              && (year `mod` 400 == 0 ||
                  year `mod` 100 /= 0)
    monthDays
      | isLeap yy = leapMonthDays
      | otherwise = normalMonthDays
    findMonth _ [] _ = error "findMonth"
    findMonth m (n:ns) z
      | z <= n    = (m,z)
      | otherwise = findMonth (m+1) ns (z-n)


normalMonthDays :: [Int]
normalMonthDays = [31,28,31,30,31,30,31,31,30,31,30,31]

leapMonthDays :: [Int]
leapMonthDays   = [31,29,31,30,31,30,31,31,30,31,30,31]

toHHMMSS :: Int -> (Int,Int,Int)
toHHMMSS x = (hh,mm,ss)
  where
    (hhmm,ss) = x `divMod` 60
    (hh,mm) = hhmm `divMod` 60
