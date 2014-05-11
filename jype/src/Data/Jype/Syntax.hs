{-# LANGUAGE OverloadedStrings #-}

module Data.Jype.Syntax
    ( Decl (..)
    , TypeName (..)
    , Body (..)
    , Value (..)
    , ConcreteType (..)
    , Field (..)
    , Choice (..)
    ) where

import Data.List
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

data Decl = Decl
    { declTypeName :: TypeName
    , declBody :: Body
    , declDescription :: [Text]
    } deriving (Eq)

instance Show Decl where
    show (Decl name body desc) = concat
        [ T.unpack . T.unlines $ map ("# " <>) desc
        , show name ++ " = " ++ show body
        ]

data TypeName = TypeName
    { typeNameConstr :: String
    , typeNameParams :: [String]
    } deriving (Eq)

instance Show TypeName where
    show (TypeName constr []) = constr
    show (TypeName constr params) = constr ++ "[" ++ intercalate ", " params ++ "]"

data Body = Object [Field] | Choice [Choice] | Primitive deriving (Eq)

instance Show Body where
    show (Object fields) = "{\n" ++ intercalate "\n" (map show fields) ++ "\n}"
    show (Choice choices) = showChoices choices
    show Primitive = "<primitive>"

data Value = NullValue | BoolValue Bool | StringValue String | IntValue Integer deriving (Eq)

instance Show Value where
    show NullValue = "null"
    show (BoolValue True) = "true"
    show (BoolValue False) = "false"
    show (StringValue str) = "\"" ++ str ++ "\"" -- show str
    show (IntValue n) = show n

data ConcreteType = ConcreteType String [ConcreteType] deriving (Eq)

instance Show ConcreteType where
    show (ConcreteType constr []) = constr
    show (ConcreteType constr params) = constr ++ "[" ++ intercalate ", " (map show params) ++ "]"

data Field = Field
    { fieldKey :: String
    , fieldType :: ConcreteType
    , fieldDescription1 :: [Text]
    , fieldDescription2 :: Maybe Text
    } deriving (Eq)

instance Show Field where
    show (Field key ty desc1 desc2) = concat
        [ T.unpack . T.unlines $ map ("  # " <>) desc1
        , "  " ++ key ++ ": " ++ show ty
        , maybe "" (T.unpack . (" # " <>)) desc2
        ]

data Choice = TypeChoice
    { choiceEither :: Either Value ConcreteType
    , choiceDescription1 :: [Text]
    , choiceDescription2 :: Maybe Text
    } deriving (Eq)

instance Show Choice where
    show (TypeChoice choice descs desc) = concat
        [ T.unpack . T.unlines $ map ("  # " <>) descs
        , "  | " ++ either show show choice
        , maybe "" (T.unpack . (" # " <>)) desc
        ]

showChoices :: [Choice] -> String
showChoices choices
    | all (\(TypeChoice _ descs desc) -> null descs && isNothing desc) choices =
        intercalate " | " $ map (either show show . choiceEither) choices
    | otherwise = "\n" ++ unlines (map show choices)
