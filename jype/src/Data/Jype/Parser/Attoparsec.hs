module Data.Jype.Parser.Attoparsec
    ( parseByteString
    , parseFile
    ) where

import Prelude hiding (readFile)
import Data.Attoparsec (Result, parse)
import Data.ByteString (ByteString, readFile)

import Data.Jype.Syntax
import Data.Jype.Parser.Internal

parseByteString :: ByteString -> Result [Decl]
parseByteString = parse decls

parseFile :: FilePath -> IO (Result [Decl])
parseFile = (fmap parseByteString) . readFile
