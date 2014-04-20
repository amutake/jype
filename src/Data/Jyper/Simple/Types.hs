module Data.Jyper.Simple.Types where

import Data.List

data Decl = Decl TypeName Body

instance Show Decl where
  show (Decl name body) = show name ++ " = " ++ show body

data TypeName = TypeName String [String]

instance Show TypeName where
  show (TypeName constr []) = constr
  show (TypeName constr params) = constr ++ "[" ++ intercalate ", " params ++ "]"

data Body = Object [Field] | Choice [ConcreteType]

instance Show Body where
  show (Object fields) = "{\n" ++ intercalate ",\n" (map (("  " ++) . show) fields) ++ "\n}"
  show (Choice types) = intercalate " | " (map show types)

data ConcreteType = ConcreteType String [ConcreteType]

instance Show ConcreteType where
  show (ConcreteType constr []) = constr
  show (ConcreteType constr params) = constr ++ "[" ++ intercalate ", " (map show params) ++ "]"

data Field = Field String ConcreteType

instance Show Field where
  show (Field key ty) = key ++ ": " ++ show ty
