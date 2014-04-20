module Data.Jyper.Types where

type TypeName = String
type TypeVariable = String

data TypeDecl = TypeDecl
  { typeIdent :: TypeIdent
  , typeBody :: Body
  } deriving (Show)

data Body
  = ObjectBody Object
  | ChoiceBody [Choice]
  deriving (Show)

data Choice = IdentChoice TypeIdent | ValueChoice Value deriving (Show)

data Value = Null
           | Boolean Bool
           | Number Int
           deriving (Show)

data Object = Object [Field] deriving (Show)

data Field = Field String TypeIdent deriving (Show)

data TypeIdent = TypeIdent TypeName [TypeVariable] deriving (Show)
