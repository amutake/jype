module Main where

import Test.Tasty
import Parser
import Checker

main :: IO ()
main = defaultMain $ testGroup "jype" $ concat
    [ parserTests
    , checkerTests
    ]
