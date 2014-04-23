{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit

import Data.Jype.Parser
import Data.Jype.Syntax

parserTests :: IO ()
parserTests = defaultMain $ testGroup "parser"
    [ testCase "no description" $ common $
        [ Decl (TypeName "user" []) (Object
            [ Field "id" (ConcreteType "int" []) [] Nothing
            , Field "name" (ConcreteType "string" []) [] Nothing
            ]) []
        , Decl (TypeName "either" ["a", "b"]) (Choice
            [ Right (ConcreteType "a" [])
            , Right (ConcreteType "b" [])
            ]) []
        ]
    , testCase "declaration description" $ common $
        [ Decl (TypeName "id" ["a"]) (Choice
            [ Right (ConcreteType "a" [])
            ]) [ "this is descriptions", "hogehoge", "yaayaa" ]
        ]
    , testCase "field description (before)" $ common $
        [ Decl (TypeName "id" ["a"]) (Object
            [ Field "a" (ConcreteType "a" []) [ "hoge", "fuga", "moge" ] Nothing
            , Field "a" (ConcreteType "a" []) [ "hoge", "fuga", "moge" ] Nothing
            , Field "a" (ConcreteType "a" []) [ "hoge", "fuga", "moge" ] Nothing
            ]) []
        ]
    , testCase "field description (after)" $ common $
        [ Decl (TypeName "id" ["a"]) (Object
            [ Field "a" (ConcreteType "a" []) [] (Just "hogehogehoge")
            , Field "a" (ConcreteType "a" []) [] (Just "hogehogehoge")
            , Field "a" (ConcreteType "a" []) [] (Just "hogehogehoge")
            ]) []
        ]
    , testCase "complex description" $ common $
        [ Decl (TypeName "hoge" ["a"]) (Object
            [ Field "hoge" (ConcreteType "a" []) ["this", "is", "hoge"] (Just "hoge")
            , Field "fuga" (ConcreteType "int" []) [] Nothing
            , Field "moge" (ConcreteType "string" []) ["this", "is", "moge"] (Just "moge")
            ]) ["this", "is", "hoge"]
        , Decl (TypeName "hoge" ["a"]) (Object
            [ Field "hoge" (ConcreteType "a" []) ["this", "is", "hoge"] Nothing
            , Field "fuga" (ConcreteType "int" []) ["this", "is", "fuga"] (Just "fuga")
            , Field "moge" (ConcreteType "string" []) [] (Just "moge")
            ]) ["this", "is", "hoge"]
        ]
    ]

common :: [Decl] -> Expectation
common decls = case parseString (unlines $ map show decls) of
    Left err -> assertFailure $ show err ++ "\n" ++ unlines (map show decls)
    Right decls' -> decls' `shouldBe` decls
