module Data.Jype.Parser.Parsec
    ( parseString
    , parseFile
    ) where

import Text.Parsec (ParseError, parse)
import Text.Parsec.String (parseFromFile)

import Data.Jype.Syntax
import Data.Jype.Parser.Internal

parseString :: String -> Either ParseError [Decl]
parseString = parse decls "from string"

parseFile :: FilePath -> IO (Either ParseError [Decl])
parseFile = parseFromFile decls
