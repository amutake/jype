module Data.Jype.Check
    ( check
    ) where

import Data.List

import Data.Jype.Syntax

check :: [Decl] -> [String]
check ds = checkTypeNames ds ++ checkKeys ds ++ checkConcreteType ds

dup :: Eq a => [a] -> [a]
dup [] = []
dup (x:xs)
    | elem x xs = x : dup (xs \\ [x])
    | otherwise = dup xs

checkTypeNames :: [Decl] -> [String]
checkTypeNames = map ("duplicate: " ++) . dup . map (typeNameConstr . declTypeName)

checkKeys :: [Decl] -> [String]
checkKeys = (>>= checkKeys')
  where
    checkKeys' (Decl name (Object fs) _) =
        map (("duplicate key in " ++ show name ++ ": ") ++) . dup . map fieldKey $ fs
    checkKeys' _  = []

checkConcreteType :: [Decl] -> [String]
checkConcreteType = const []
