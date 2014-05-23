module Data.Jype
    ( module Data.Jype.Check
    , module Data.Jype.Error
    , module Data.Jype.Parser
    , module Data.Jype.Syntax
    , module Data.Jype.Primitives
    , parseWithCheck
    ) where

import Data.Jype.Check
import Data.Jype.Error
import Data.Jype.Parser
import Data.Jype.Syntax
import Data.Jype.Primitives

parseWithCheck :: String -> Either JypeError [Decl]
parseWithCheck = either (Left . JypeParseError . show) (check . (++ primitives)) . parseString
