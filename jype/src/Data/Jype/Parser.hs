{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-do-bind -fno-warn-unused-matches #-}

module Data.Jype.Parser
    ( parseFile
    , parseString
    ) where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Numeric
import Text.Peggy (ParseError, peggy, defaultDelimiter)
-- import Text.Peggy (space)
import qualified Text.Peggy as P

import Data.Jype.Syntax

[peggy|
decls :: [Decl]
    = decl* !.

decl :: Decl
    = d:description* newline n:name newline "=" newline b:body newline { Decl n b d }

description ::: Text
    = '#' space* (!'\n' .)* '\n' { T.pack $2 }

name :: TypeName
    = i:ident newline "[" p:ident newline ps:("," ident)* "]" { TypeName i (p : ps) }
    / i:ident newline { TypeName i [] }

ident ::: String = [a-zA-Z] [a-zA-Z0-9_]* { $1 : $2 }

body :: Body
    = "{" newline fs:field* newline "}" { Object fs }
    / v:valueOrType newline vs:("|" valueOrType)* { Choice (v : vs) }

valueOrType :: Either Value ConcreteType
    = v:value { Left v }
    / c:concrete { Right c }

value :: Value
    = "null" { NullValue }
    / "true" { BoolValue True }
    / "false" { BoolValue False }
    / s:string { StringValue s }
    / n:int { IntValue n }

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
    = i:ident "[" c:concrete cs:("," concrete)* "]" { ConcreteType i (c : cs) }
    / ident { ConcreteType $1 [] }

field :: Field
    = ds:description* i:ident ":" c:concrete d:description? newline { Field i c ds d }

newline :: () = [ \t\r\n]* { () }
space :: () = [ \t] { () }
|]

parseFile :: FilePath -> IO (Either ParseError [Decl])
parseFile = P.parseFile decls

parseString :: String -> Either ParseError [Decl]
parseString = P.parseString decls "string"
