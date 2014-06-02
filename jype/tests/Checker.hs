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
