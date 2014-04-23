{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import System.Directory
import System.Environment
import Text.Blaze.Html.Renderer.Utf8

import Data.Jype.Parser
import Data.Jype.Types

import Text.Jype.Html

main :: IO ()
main = do
    [htmlDir, jypeFile] <- getArgs
    jype <- parse jypeFile
    either print (generate htmlDir) jype

generate :: FilePath -> [Decl] -> IO ()
generate dir decls = do
    createDirectoryIfMissing True dir
    let css = $(embedFile "static/jype.css")
    BS.writeFile (dir ++ "/jype.css") css
    BL.writeFile (dir ++ "/jype.html") $ renderHtml $ html decls
