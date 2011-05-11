{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Date.Formatter (formatHTTPDate) where

import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Date.Types

----------------------------------------------------------------

formatHTTPDate :: HTTPDate -> ByteString
formatHTTPDate hd = BS.concat [
      week, ", "
    , day, " ", month, " ", year, " "
    , hh, ":", mm, ":", ss, " "
    , "GMT"
    ]
  where
    y = hdYear hd
    m = hdMonth hd
    d = hdDay hd
    h = hdHour hd
    n = hdMinute hd
    s = hdSecond hd
    w = hdWkday hd
    week = weekDays ! w
    year = toB y
    month = months ! m
    day = pad d
    hh = pad h
    mm = pad n
    ss = pad s
    toB = BS8.pack . show
    to2B = BS8.pack . ('0' :) . show
    pad x = if x < 10 then to2B x else toB x

--------------------------------

months :: Array Int ByteString
months = listArray (1,12) [
      "Jan", "Feb", "Mar"
    , "Apr", "May", "Jun"
    , "Jul", "Aug", "Sep"
    , "Oct", "Nov", "Dec"
    ]

weekDays :: Array Int ByteString
weekDays = listArray (1,7) [
      "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"
    ]
