{-# LANGUAGE DeriveDataTypeable #-}

module Data.Jype.Error
    ( JypeError (..)
    ) where

import Control.Exception
import Data.Typeable

data JypeError = JypeParseError String
               | JypeCheckError [String]
               deriving (Show, Typeable)

instance Exception JypeError
