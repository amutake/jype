{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit

import Data.Jype.Parser
import Data.Jype.Syntax

parserTests :: IO ()
parserTests = defaultMain $ testGroup "parser"
    [ testGroup "parse . show == id" $
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
    , testGroup "parse == decls" $
        [ testCase "complex" $ parseEqual "./tests/jypefiles/1.jype" $
            [ Decl (TypeName "hoge" ["a","b","c"]) (Object
                [ Field "key" (ConcreteType "type" []) [] (Just "hoge")
                , Field "key" (ConcreteType "a" [ConcreteType "a" []]) ["hoge"] Nothing
                , Field "key" (ConcreteType "either" [ConcreteType "b" [], ConcreteType "c" []]) ["hoge"] Nothing
                ]) ["hoge   # hoge", "hoge", "hoge"]
            , Decl (TypeName "hoge" []) (Choice
                [ Left (IntValue (-1))
                , Left (StringValue "hoge")
                , Left (IntValue 1)
                , Left (BoolValue True)
                , Left (BoolValue False)
                ]) ["hoge", "hoge"]
            ]
        , testCase "newlines" $ parseEqual "./tests/jypefiles/2.jype" $
            [ Decl (TypeName "tree" ["a"]) (Object
                [ Field "root" (ConcreteType "a" []) ["root"] (Just "root")
                , Field "forest" (ConcreteType "forest" [ConcreteType "a" []]) [] (Just "forest")
                ]) []
            , Decl (TypeName "forest" ["a"]) (Choice
                [ Right (ConcreteType "array" [ConcreteType "tree" [ConcreteType "a" []]])
                ]) []
            , Decl (TypeName "either" ["a", "b"]) (Choice
                [ Right (ConcreteType "a" [])
                , Right (ConcreteType "b" [])
                ]) ["either"]
            ]
        , testCase "no newlines" $ parseEqual "./tests/jypefiles/3.jype" $
            [ Decl (TypeName "tree" ["a"]) (Object
                [ Field "root" (ConcreteType "a" []) ["root"] (Just "root")
                , Field "forest" (ConcreteType "forest" [ConcreteType "a" []]) [] Nothing
                ]) []
            , Decl (TypeName "forest" ["a"]) (Choice
                [ Right (ConcreteType "array" [ConcreteType "tree" [ConcreteType "a" []]])
                ]) []
            , Decl (TypeName "either" ["a", "b"]) (Choice
                [ Right (ConcreteType "a" [])
                , Right (ConcreteType "b" [])
                ]) ["either"]
            ]
        ]
    ]

common :: [Decl] -> Expectation
common decls = case parseString (unlines $ map show decls) of
    Left err -> assertFailure $ show err ++ show (Decls decls)
    Right decls' -> Decls decls' `shouldBe` Decls decls

newtype Decls = Decls [Decl] deriving Eq

instance Show Decls where
    show (Decls decls) = ("\n" ++) . unlines . map show $ decls

parseEqual :: FilePath -> [Decl] -> Expectation
parseEqual path decls = do
    result <- parseFile path
    case result of
        Left err -> assertFailure $ show err ++ show (Decls decls)
        Right decls' -> Decls decls' `shouldBe` Decls decls
