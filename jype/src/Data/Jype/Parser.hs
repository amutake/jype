{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.Jype.Parser where

import Data.Char
import Numeric
import Text.Peggy (parseFile, ParseError, peggy, defaultDelimiter, space)

import Data.Jype.Types

[peggy|
decls :: [Decl]
  = decl*

decl :: Decl
  = name "=" body { Decl $1 $2 }

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
  = field ("," field)* { $1 : $2 }

field :: Field
  = ident ":" concrete { Field $1 $2 }
|]

parse :: FilePath -> IO (Either ParseError [Decl])
parse = parseFile decls
