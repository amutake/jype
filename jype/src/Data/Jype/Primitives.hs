{-# LANGUAGE OverloadedStrings #-}

module Data.Jype.Primitives
    ( primitives
    , primitives'
    , test
    , lang
    ) where

import Data.Jype.Syntax
import qualified Data.Jype.AST as AST

primitives :: [Decl]
primitives =
    [ Decl (TypeName "int" []) Primitive ["Integer type"]
    , Decl (TypeName "float" []) Primitive ["Float type"]
    , Decl (TypeName "string" []) Primitive ["String type"]
    , Decl (TypeName "array" ["a"]) Primitive ["Array type of type 'a'"]
    , Decl (TypeName "option" ["a"]) Primitive ["option[a] represents either just type 'a' or there is no value."]
    ]

primitives' :: [AST.Jype]
primitives' =
    [ int
    , AST.Jype $ AST.Description ["float"] $ AST.Type "float" [] AST.Primitive
    , AST.Jype $ AST.Description ["string"] $ AST.Type "string" [] AST.Primitive
    , AST.Jype $ AST.Description ["array of a"] $ AST.Type "array" ["a"] AST.Primitive
    , AST.Jype $ AST.Description ["option of a"] $ AST.Type "option" ["a"] AST.Primitive
    ]

int :: AST.Jype
int = AST.Jype $ AST.Description ["integer"] $ AST.Type "int" [] AST.Primitive

test :: AST.Jype
test = AST.Jype $ AST.Description ["this", "is", "test"] $ AST.Type "user" ["a"] $ AST.Record
    [ AST.Description ["this", "is", "id"] $ AST.Field "id" $ AST.CompleteType (AST.VTType int) []
    , AST.Description [] $ AST.Field "self" $ AST.CompleteType (AST.VTType test) [AST.CompleteType (AST.VTVar "a") []]
    , AST.Description [] $ AST.Field "lang" $ AST.CompleteType (AST.VTType lang) []
    ]

lang :: AST.Jype
lang = AST.Jype $ AST.Description ["this", "is", "lang"] $ AST.Type "language" [] $ AST.Union
    [ AST.Description [] $ AST.ValueChoice $ AST.StringValue "ja"
    , AST.Description ["English"] $ AST.ValueChoice $ AST.StringValue "en"
    , AST.Description [] $ AST.TypeChoice $ AST.CompleteType (AST.VTType test) []
    ]
