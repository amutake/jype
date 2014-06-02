{-# LANGUAGE DeriveDataTypeable #-}

module Data.Jype.Error
    ( JypeError (..)
    ) where

import Control.Exception
import Data.List (intercalate)
import Data.Typeable

data JypeError = JypeParseError String
               | JypeCheckError [String]
               deriving (Typeable, Eq)

instance Show JypeError where
    show (JypeParseError err) = "JypeParseError: " ++ err
    show (JypeCheckError errs) = "JypeCheckError:\n  " ++ intercalate "\n  " errs

instance Exception JypeError
