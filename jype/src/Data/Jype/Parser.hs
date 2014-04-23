{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-do-bind -fno-warn-unused-matches #-}

module Data.Jype.Parser
    ( parseFile
    , parseString
    ) where

import Data.Char
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import Numeric
import Text.Peggy (ParseError, peggy, defaultDelimiter, space)
import qualified Text.Peggy as P

import Data.Jype.Syntax

[peggy|
decls :: [Decl]
    = decl* !.

decl :: Decl
    = name "=" body { Decl $1 $2 [] }
    / descriptions name "=" body { Decl $2 $3 $1 }

descriptions :: [Text]
    = description description* { $1 : $2 }

description ::: Text
    = '#' skipSpaces (!'\n' .)* '\n' { T.pack $2 }

name :: TypeName
    = ident "[" ident ("," ident)* "]" { TypeName $1 ($2 : $3) }
    / ident { TypeName $1 [] }

ident ::: String = [a-zA-Z] [a-zA-Z0-9_]* { $1 : $2 }

body :: Body
    = "{" "}" { Object [] }
    / "{" fields "}" { Object $1 }
    / valueOrType ("|" valueOrType)* { Choice ($1 : $2) }

valueOrType :: Either Value ConcreteType
    = value { Left $1 }
    / concrete { Right $1 }

value :: Value
    = "null" { NullValue }
    / "true" { BoolValue True }
    / "false" { BoolValue False }
    / string { StringValue $1 }
    / int { IntValue $1 }

string ::: String
    = '\"' char* '\"'

char :: Char
    = '\\' escChar
    / [^\"\\]

escChar :: Char
    = '\"' { '\"' }
    / '\\' { '\\' }
    / '/' { '/' }
    / 'b' { '\b' }
    / 'f' { '\f' }
    / 'n' { '\n' }
    / 'r' { '\r' }
    / 't' { '\t' }
    / 'u' hex hex hex hex { chr $ fst $ head $ readHex [$1, $2, $3, $4] }

hex :: Char = [0-9a-zA-Z]

int :: Int
    = "0" { 0 }
    / [1-9] [0-9]* { read ($1 : $2) }
    / "-" [1-9] [0-9]* { negate (read ($1 : $2)) }

concrete :: ConcreteType
    = ident "[" concrete ("," concrete)* "]" { ConcreteType $1 ($2 : $3) }
    / ident { ConcreteType $1 [] }

fields :: [Field]
    = field field* { $1 : $2 }

field :: Field
    = descriptions? ident ":" concrete description? { Field $2 $3 (F.concat $1) $4 }

skipSpaces :: () = [ \t]* { () }
|]

parseFile :: FilePath -> IO (Either ParseError [Decl])
parseFile = P.parseFile decls

parseString :: String -> Either ParseError [Decl]
parseString = P.parseString decls "string"
