module Checker where

import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit

import Data.Jype

checkerTests :: [TestTree]
checkerTests =
    [ testGroup "pass check" $
        [ testCase "simple" $ checkSuccess "./tests/jypefiles/checker1.jype"
        , testCase "complex" $ checkSuccess "./tests/jypefiles/checker2.jype"
        ]
    , testGroup "fail check" $
        [ testCase "1" $ checkFailure "./tests/jypefiles/checker_fail1.jype" $ \e ->
           e == JypeCheckError ["duplicate key in type 'user': id"]
        , testCase "2" $ checkFailure "./tests/jypefiles/checker_fail2.jype" $ \e ->
           e == JypeCheckError ["duplicate type declaration: user"]
        , testCase "3" $ checkFailure "./tests/jypefiles/checker_fail3.jype" $ \e ->
           e == JypeCheckError
               [ "duplicate type/value in type 'a[b]': int"
               , "duplicate type/value in type 'a[b]': \"hoge\""
               , "duplicate type/value in type 'a[b]': b"
               ]
        , testCase "4" $ checkFailure "./tests/jypefiles/checker_fail4.jype" $ \e ->
           e == JypeCheckError
               [ "unknown type in type 'hoge': fuga"
               , "unknown type in type 'a': b"
               ]
        , testCase "5" $ checkFailure "./tests/jypefiles/checker_fail5.jype" $ \e ->
           e == JypeCheckError
                [ "wrong number of arguments of type constructor 'a': expected 0 but got 1"
                , "wrong number of arguments of type constructor 'option': expected 1 but got 0"
                ]
        ]
    ]

checkSuccess :: FilePath -> Expectation
checkSuccess path = do
    result <- parseFile path
    case result of
        Left err -> assertFailure $ show err
        Right decls -> case check (decls ++ primitives) of
            Left err -> assertFailure $ show err
            Right _ -> assertBool "" True

checkFailure :: FilePath -> (JypeError -> Bool) -> Expectation
checkFailure path p = do
    result <- parseFile path
    case result of
        Left err -> assertFailure $ show err
        Right decls -> case check (decls ++ primitives) of
            Left err -> err `shouldSatisfy` p
            Right decls' -> assertFailure $ unlines $ map show decls'
