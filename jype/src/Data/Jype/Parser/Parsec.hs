module Data.Jype.Parser.Parsec
    ( parseString
    , parseByteString
    , parseFile
    , ParseError
    ) where

import Data.ByteString (ByteString)
import Text.Parsec (ParseError, parse)
import Text.Parsec.String (parseFromFile)

import Data.Jype.Syntax
import Data.Jype.Parser.Internal

parseString :: String -> Either ParseError [Decl]
parseString = parse decls "from string"

parseByteString :: ByteString -> Either ParseError [Decl]
parseByteString = parse decls "from bytestring"

parseFile :: FilePath -> IO (Either ParseError [Decl])
parseFile = parseFromFile decls
