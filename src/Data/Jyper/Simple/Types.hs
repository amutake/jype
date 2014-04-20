module Data.Jyper.Simple.Types where

import Data.List

data Decl = Decl String Body

instance Show Decl where
  show (Decl ident body) = ident ++ " = " ++ show body

data Body = Object [Field] | Choice [String]

instance Show Body where
  show (Object fields) = "{\n" ++ intercalate ",\n" (map (("  " ++) . show) fields) ++ "\n}"
  show (Choice types) = intercalate " | " types

data Field = Field String String

instance Show Field where
  show (Field key ty) = key ++ ": " ++ ty
