module Data.Jype.Primitives
    ( primitives
    ) where

import Data.Jype.Syntax

primitives :: [Decl]
primitives =
    [ Decl (TypeName "int" []) Primitive []
    , Decl (TypeName "string" []) Primitive []
    , Decl (TypeName "array" ["a"]) Primitive []
    , Decl (TypeName "bool" []) (Choice
        [ TypeChoice (Left (BoolValue False)) [] Nothing
        , TypeChoice (Left (BoolValue True)) [] Nothing
        ]) []
    , Decl (TypeName "nullable" ["a"]) (Choice
        [ TypeChoice (Left NullValue) [] Nothing
        , TypeChoice (Right (ConcreteType "a" [])) [] Nothing
        ]) []
    ]
