{-# LANGUAGE OverloadedStrings #-}

module Data.Jype.Primitives
    ( primitives
    ) where

import Data.Jype.Syntax

primitives :: [Decl]
primitives =
    [ Decl (TypeName "int" []) Primitive ["Integer type"]
    , Decl (TypeName "string" []) Primitive ["String type"]
    , Decl (TypeName "array" ["a"]) Primitive ["Array type of type 'a'"]
    , Decl (TypeName "option" ["a"]) Primitive ["option[a] represents either a type 'a' or there is no key."]
    ]
