{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Data.Jyper.Simple.Parser where

import Text.Peggy hiding (parse, Choice)

import Data.Jyper.Simple.Types

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
  / concrete ("|" concrete)* { Choice ($1 : $2) }

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
