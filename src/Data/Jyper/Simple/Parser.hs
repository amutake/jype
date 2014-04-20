{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Data.Jyper.Simple.Parser where

import Text.Peggy hiding (parse, Choice)

import Data.Jyper.Simple.Types

[peggy|
decls :: [Decl]
  = decl*

decl :: Decl
  = ident "=" body { Decl $1 $2 }

ident :: String = [a-zA-Z] [a-zA-Z0-9_]* { $1 : $2 }

body :: Body
  = "{" "}" { Object [] }
  / "{" fields "}" { Object $1 }
  / ident ("|" ident)* { Choice ($1 : $2) }

fields :: [Field]
  = field ("," field)* { $1 : $2 }

field :: Field
  = ident ":" ident { Field $1 $2 }
|]

parse :: FilePath -> IO (Either ParseError [Decl])
parse = parseFile decls
