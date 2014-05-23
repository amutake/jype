module Data.Jype.Check
    ( check
    ) where

import Data.List

import Data.Jype.Error
import Data.Jype.Syntax

check :: [Decl] -> Either JypeError [Decl]
check ds
    | null errors = Right ds
    | otherwise = Left $ JypeCheckError errors
  where
    errors = checkTypeNames ds ++ checkKeys ds ++ checkConcreteType ds

dup :: Eq a => [a] -> [a]
dup [] = []
dup (x:xs)
    | elem x xs = x : dup (xs \\ [x])
    | otherwise = dup xs

checkTypeNames :: [Decl] -> [String]
checkTypeNames = map ("duplicate type: " ++) . dup . map (typeNameConstr . declTypeName)

checkKeys :: [Decl] -> [String]
checkKeys = (>>= checkKeys')
  where
    checkKeys' (Decl name (Object fs) _) =
        map (("duplicate key in type " ++ show name ++ ": ") ++) . dup . map fieldKey $ fs
    checkKeys' _  = []

checkConcreteType :: [Decl] -> [String]
checkConcreteType = const []
