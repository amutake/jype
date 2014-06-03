module Data.Jype.Aeson
    (
    ) where

import Data.Aeson
import qualified Data.Aeson as A

import Data.Jype

isValueOf :: Value -> Decl -> Bool
isValueOf (Object obj) decl = undefined

typeErrors :: Value -> Decl -> [String]
