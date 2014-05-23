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
    errors = checkTypeNames ds ++ checkKeys ds ++ checkUnknownType ds

dup :: Eq a => [a] -> [a]
dup [] = []
dup (x:xs)
    | elem x xs = x : dup (xs \\ [x])
    | otherwise = dup xs

-- | Check whether the name that decleared type is duplicated
checkTypeNames :: [Decl] -> [String]
checkTypeNames = map ("duplicate type: " ++) . dup . map (typeNameConstr . declTypeName)

-- | Check whether keys in fields are duplicated
checkKeys :: [Decl] -> [String]
checkKeys = (>>= checkKeys')
  where
    checkKeys' (Decl name (Object fs) _) =
        map (("duplicate key in type " ++ show name ++ ": ") ++) . dup . map fieldKey $ fs
    checkKeys' _  = []

-- | Check whether concrete types are defined
checkUnknownType :: [Decl] -> [String]
checkUnknownType decls = decls >>= checkDecl
  where
    names = map (typeNameConstr . declTypeName) decls
    checkDecl (Decl typ@(TypeName _ params) (Object fields) _) =
        fields >>= (checkConc (show typ) params . fieldType)
    checkDecl (Decl typ@(TypeName _ params) (Choice choices) _) =
        choices >>= (checkChoice (show typ) params . choiceEither)
    checkDecl (Decl _ Primitive _) = []
    checkConc typ params (ConcreteType name paramConcs)
        | elem name (names ++ params) = paramConcs >>= checkConc typ params
        | otherwise = ("unknown type in type " ++ typ ++ ": " ++ name) : (paramConcs >>= checkConc typ params)
    checkChoice _ _ (Left _) = []
    checkChoice typ params (Right conc) = checkConc typ params conc
