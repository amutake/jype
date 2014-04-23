module Data.Jype.Primitives
  ( primitives
  ) where

import Data.Jype.Types

primitives :: [Decl]
primitives =
  [ Decl (TypeName "int" []) Primitive
  , Decl (TypeName "string" []) Primitive
  , Decl (TypeName "array" ["a"]) Primitive
  , Decl (TypeName "bool" []) (Choice [Left (BoolValue False), Left (BoolValue True)])
  , Decl (TypeName "nullable" ["a"]) (Choice [Left NullValue, Right (ConcreteType "a" [])])
  ]
