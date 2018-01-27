module Main where

import Protolude
import Test.DocTest

main :: IO ()
main = doctest ["src", "-XNoImplicitPrelude"]
