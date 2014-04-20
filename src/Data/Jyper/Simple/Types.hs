module Data.Jyper.Simple.Types where

import Data.List

data Decl = Decl TypeName Body

instance Show Decl where
  show (Decl name body) = show name ++ " = " ++ show body

data TypeName = TypeName String [String]

instance Show TypeName where
  show (TypeName constr []) = constr
  show (TypeName constr params) = constr ++ "[" ++ intercalate ", " params ++ "]"

data Body = Object [Field] | Choice [Either Value ConcreteType]

instance Show Body where
  show (Object fields) = "{\n" ++ intercalate ",\n" (map (("  " ++) . show) fields) ++ "\n}"
  show (Choice types) = intercalate " | " (map (either show show) types)

data Value = NullValue | BoolValue Bool | StringValue String | IntValue Int

instance Show Value where
  show NullValue = "null"
  show (BoolValue True) = "true"
  show (BoolValue False) = "false"
  show (StringValue str) = "\"" ++ str ++ "\"" -- show str
  show (IntValue n) = show n

data ConcreteType = ConcreteType String [ConcreteType]

instance Show ConcreteType where
  show (ConcreteType constr []) = constr
  show (ConcreteType constr params) = constr ++ "[" ++ intercalate ", " (map show params) ++ "]"

data Field = Field String ConcreteType

instance Show Field where
  show (Field key ty) = key ++ ": " ++ show ty
