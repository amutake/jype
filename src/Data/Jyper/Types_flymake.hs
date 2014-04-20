module Data.Jyper.Types where

type TypeName = String
type TypeVariable = String

data TypeDecl = TypeDecl
  { typeIdent :: TypeIdent
  , typeBody :: Body
  }

data Body
  = ObjectBody Object
  | ChoiceBody [Choice]

data Choice = IdentChoice TypeIdent | ValueChoice Value

data Value = Null
           | Boolean Bool
           | Number Int

data Object = Object [Field]

data Field = Field String TypeIdent

data TypeIdent = TypeIdent TypeName [TypeVariable]
