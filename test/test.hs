{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Model
import Network.HTTP.Date

infixr 5 +++

(+++) :: ByteString -> ByteString -> ByteString
(+++) = BS.append

main :: IO ()
main = do
    test1
    test2

test1 :: IO ()
test1 = do
    BS.putStr "*** Testing 'formatHTTPDate' against its model...\n"
    mapM_ test [0,100000..10000000000]
  where
    model = utcToDate . epochTimeToUtcTime
    ours  = formatHTTPDate . epochTimeToHTTPDate
    test epochtime = do
        let m = model epochtime
            o = ours epochtime
        when (m /= o) $ do
            BS.putStr $ "Model: " +++ m +++ "\n"
            BS.putStr $ "Ours : " +++ o +++ "\n"

test2 :: IO ()
test2 = do
    BS.putStr "*** Testing 'parseHTTPDate' against 'formatHTTPDate'...\n"
    mapM_ test [0,100000..10000000000]
  where
    test epochtime = do
        let hd = epochTimeToHTTPDate epochtime
            bs = formatHTTPDate hd
            mhd = parseHTTPDate bs
        case mhd of
            Nothing  -> BS.putStr $ "parse fail: " +++ bs +++ "\n"
            Just hd' -> when (hd /= hd') $ do
                putStrLn $ "Model: " ++ show hd
                putStrLn $ "Ours : " ++ show hd'
