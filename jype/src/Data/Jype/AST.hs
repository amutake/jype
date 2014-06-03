{-# LANGUAGE OverloadedStrings #-}

module Data.Jype.AST where

import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

newtype Jype = Jype (Description Type) deriving (Eq)

data Type = Type
    { typeName :: String
    , typeArguments :: [String]
    , typeBody :: Body
    } deriving (Eq)

data Body = Record { recordFields :: [Description Field] }
          | Union { unionConstrs :: [Description Choice] }
          | Primitive
          deriving (Eq)

data Description a = Description
    { description :: [Text]
    , content :: a
    } deriving (Eq)

data Field = Field
    { fieldKey :: String
    , fieldType :: CompleteType
    } deriving (Eq)

data Choice = ValueChoice { valueChoice :: Value }
            | TypeChoice { typeChoice :: CompleteType }
            deriving (Eq)

data CompleteType = CompleteType
    { compConstr :: VarType
    , compArguments :: [CompleteType]
    } deriving (Eq)

data VarType = VTVar { vtVar :: String }
             | VTType { vtType :: Jype }
             deriving (Eq)

data Value = NullValue
           | BoolValue Bool
           | StringValue String
           | IntValue Integer
           deriving (Eq)

instance Show Jype where
    show (Jype d) = showDesc False d

instance Show a => Show (Description a) where
    show d = showDesc False d

showDesc :: Show a => Bool -> Description a -> String
showDesc b (Description d c) = concat
    [ T.unpack . T.unlines $ map ((indent b <> "# ") <>) d
    , show c
    ]
  where
    indent True = "  "
    indent False = ""

instance Show Type where
    show (Type name args body) = name ++ showArgs args ++ " = " ++ show body
      where
        showArgs [] = ""
        showArgs a = "[" ++ intercalate ", " a ++ "]"

instance Show Body where
    show (Record fields) = "{\n" ++ unlines (map (showDesc True) fields) ++ "}"
    show (Union choices) = "\n" ++ (intercalate "\n" $ map (showDesc True) choices)
    show Primitive = "<primitive>"

instance Show Field where
    show (Field key typ) = "  " ++ key ++ ": " ++ show typ

instance Show Choice where
    show (ValueChoice v) = "  | " ++ show v
    show (TypeChoice t) = "  | " ++ show t

instance Show CompleteType where
    show (CompleteType vt vts) = show vt ++ showArgs vts
      where
        showArgs [] = ""
        showArgs args = "[" ++ intercalate ", " (map show args) ++ "]"

instance Show VarType where
    show (VTVar v) = v
    show (VTType (Jype (Description _ (Type n _ _)))) = n

instance Show Value where
    show NullValue = "null"
    show (BoolValue True) = "true"
    show (BoolValue False) = "false"
    show (StringValue str) = "\"" ++ str ++ "\""
    show (IntValue n) = show n
