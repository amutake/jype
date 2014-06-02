{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import System.Directory
import System.Environment
import Text.Blaze.Html.Renderer.Utf8

import Data.Jype.Parser (parseFile)
import Data.Jype.Primitives (primitives)
import Data.Jype.Syntax (Decl)

import Text.Jype.Html

main :: IO ()
main = do
    args <- getArgs
    case args of
        [htmlDir, jypeFile] -> do
            jype <- parseFile jypeFile
            either print (generate htmlDir . (primitives ++)) $ jype
        _ -> do
            putStrLn "Usage: jype-doc <dir> <jype-file>"

generate :: FilePath -> [Decl] -> IO ()
generate dir decls = do
    createDirectoryIfMissing True dir
    let css = $(embedFile "static/jype.css")
    BS.writeFile (dir ++ "/jype.css") css
    BL.writeFile (dir ++ "/jype.html") $ renderHtml $ html decls
