module Data.Jype
    ( module Data.Jype.Check
    , module Data.Jype.Error
    , module Data.Jype.Parser
    , module Data.Jype.Syntax
    , module Data.Jype.Primitives
    , parseStringWithCheck
    , parseFileWithCheck
    ) where

import Data.Jype.Check
import Data.Jype.Error
import Data.Jype.Parser
import Data.Jype.Syntax
import Data.Jype.Primitives

withCheck :: Show e => Either e [Decl] -> Either JypeError [Decl]
withCheck = either (Left . JypeParseError . show) (check . (primitives ++))

parseStringWithCheck :: String -> Either JypeError [Decl]
parseStringWithCheck = withCheck . parseString

parseFileWithCheck :: FilePath -> IO (Either JypeError [Decl])
parseFileWithCheck = (withCheck `fmap`) `fmap` parseFile
