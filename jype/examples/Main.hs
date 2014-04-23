module Main where

import System.Environment

import Data.Jype.Parser

main :: IO ()
main = do
  path <- head `fmap` getArgs
  result <- parseFile path
  case result of
    Left err -> print err
    Right ds -> mapM_ print ds
