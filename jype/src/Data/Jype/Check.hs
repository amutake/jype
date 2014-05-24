module Data.Jype.Check
    ( check
    ) where

import Control.Arrow ((&&&))
import Data.List
import qualified Data.Map as M

import Data.Jype.Error
import Data.Jype.Syntax

check :: [Decl] -> Either JypeError [Decl]
check ds
    | null errors = Right ds
    | otherwise = Left $ JypeCheckError errors
  where
    errors = nub $ checkTypeNames ds ++ checkKeys ds ++ checkUnknownType ds ++ checkArgLen ds

dup :: Eq a => [a] -> [a]
dup [] = []
dup (x:xs)
    | elem x xs = x : dup (xs \\ [x])
    | otherwise = dup xs

-- | Check whether the name that decleared type is duplicated
checkTypeNames :: [Decl] -> [String]
checkTypeNames = map ("duplicate type declaration: " ++) . dup . map (typeNameConstr . declTypeName)

-- | Check whether keys in fields are duplicated
checkKeys :: [Decl] -> [String]
checkKeys = (>>= checkKeys')
  where
    checkKeys' (Decl name (Object fs) _) =
        map (("duplicate key in type '" ++ show name ++ "': ") ++) . dup . map fieldKey $ fs
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
        | otherwise = ("unknown type in type '" ++ typ ++ "': " ++ name) : (paramConcs >>= checkConc typ params)
    checkChoice _ _ (Left _) = []
    checkChoice typ params (Right conc) = checkConc typ params conc

checkArgLen :: [Decl] -> [String]
checkArgLen decls = decls >>= checkDecl
  where
    nameLenMap = M.fromList
               $ map (typeNameConstr &&& (length . typeNameParams))
               $ map declTypeName decls
    typeParams = M.union nameLenMap . M.fromList . flip zip (repeat 0)
    checkDecl (Decl (TypeName _ params) (Object fields) _) = fields >>= (checkConc params . fieldType)
    checkDecl (Decl (TypeName _ params) (Choice choices) _) = choices >>= (checkChoice params . choiceEither)
    checkDecl (Decl _ Primitive _) = []
    checkConc tyvars (ConcreteType name params) = case M.lookup name (typeParams tyvars) of
        Just n | length params /= n -> concat
            [ "wrong number of arguments of type constructor '"
            , name
            , "': expected "
            , show n
            , " but got "
            , show (length params)
            ] : (params >>= checkConc tyvars)
        _ -> params >>= checkConc tyvars
    checkChoice _ (Left _) = []
    checkChoice params (Right t) = checkConc params t
