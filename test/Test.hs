{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Model
import Network.HTTP.Date
import Test.Framework.Providers.HUnit
import Test.Framework.TH.Prime
import Test.HUnit
import Test.DocTest

main :: IO ()
main = $(defaultMainGenerator)

----------------------------------------------------------------

case_doc_test :: Assertion
case_doc_test = doctest ["Network/HTTP/Date", "-XOverloadedStrings"]

----------------------------------------------------------------

case_formatHTTPDate :: Assertion
case_formatHTTPDate = do
    forM_ [0,100000..10000000000] $ \epochtime -> do
        let m = model epochtime
            o = ours epochtime
        o @?= m
  where
    model = utcToDate . epochTimeToUtcTime
    ours  = formatHTTPDate . epochTimeToHTTPDate

case_parseHTTPDate :: Assertion
case_parseHTTPDate = do
    forM_ [0,100000..10000000000] $ \epochtime -> do
        let m = epochTimeToHTTPDate epochtime
            Just o = parseHTTPDate $ formatHTTPDate m
        o @?= m
