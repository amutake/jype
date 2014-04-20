{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Data.Jyper.Parser where

import Text.Peggy hiding (parse)

import Data.Jyper.Types

[peggy|
decls :: [TypeDecl]
  = decl*

decl :: TypeDecl
  = typeident "=" body { TypeDecl $1 $2 }

name :: TypeName = [a-zA-Z_][0-9a-zA-Z_'-]*

body :: Body
  = object { ObjectBody $1 }
  / choice ("|" choice)* { ChoiceBody ($1 : $2) }

object :: Object
  = "{" "}" { Object [] }
  / "{" fields "}" { Object $1 }

fields :: [Field]
  = field ("," field)* { $1 : $2 }

field :: Field
  = name ":" typeident { Field $1 #2 }

typeident :: TypeIdent
  = name { TypeIdent $1 [] }
  / name "[" name ("," name)* "]" { TypeIdent $1 ($2 : $3) }

choice :: Choice
  = typeident { IdentChoice $1 }
  / value { ValueChoice $1 }

value :: Value
  = "null" { Null }
  / bool { Boolean $1 }
  / number { Number $1 }

bool :: Bool
  = "true" { True }
  / "false" { False }

number :: Int
  = [0-9]+ { read $1 }
|]

parse :: FilePath -> IO (Either ParseError [TypeDecl])
parse = parseFile decls
