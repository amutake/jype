module Main where

import Control.Monad
import System.Environment

import Data.Jyper.Simple.Parser

main :: IO ()
main = do
  path <- head `fmap` getArgs
  result <- parse path
  case result of
    Left err -> print err
    Right ds -> mapM_ print ds
