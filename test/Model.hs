module Model where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Time
import Data.Time.Clock.POSIX
import System.Locale
import System.Posix.Types

epochTimeToUtcTime :: EpochTime -> UTCTime
epochTimeToUtcTime = posixSecondsToUTCTime . realToFrac

utcToDate :: UTCTime -> ByteString
utcToDate = BS8.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"
