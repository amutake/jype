module Main where

import System.Environment

import Data.Jype

main :: IO ()
main = do
    path <- head `fmap` getArgs
    result <- parseFileWithCheck path
    case result of
        Left err -> print err
        Right ds -> mapM_ (\d -> print d >> putStrLn "") ds
