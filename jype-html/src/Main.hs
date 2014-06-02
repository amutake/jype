{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import Options.Applicative hiding (ParseError)
import System.Directory
import Text.Blaze.Html.Renderer.Utf8

import Data.Jype.Parser (parseFile, parseByteString, ParseError)
import Data.Jype.Primitives (primitives)
import Data.Jype.Syntax (Decl)
import Data.Jype.Check (check)

import Text.Html.Jype

data Config = Config
    { configDeployDir :: FilePath
    , configJypeFiles :: FilePath
    , configWithPrelude :: Bool
    } deriving (Eq, Show)

configParser :: Parser Config
configParser = Config
    <$> strOption (long "deploy" <> short 'd' <> metavar "DEPLOY_DIR")
    <*> strOption (long "jype" <> short 'j' <> metavar "JYPEFILE")
    <*> flag False True (long "prelude" <> short 'p')

parseFiles :: FilePath -> IO (Either ParseError [Decl])
parseFiles path = do
    e <- doesDirectoryExist path
    parseDir e
  where
    parseDir True = do
        paths <- filter (\p -> p /= "." && p /= "..") <$> getDirectoryContents path
        (fmap concat . sequence) <$> mapM parseFile paths
    parseDir False = parseFile path

main :: IO ()
main = do
    config <- execParser opts
    result <- parseFiles $ configJypeFiles config
    either print (generate config . appPrim (configWithPrelude config)) result
  where
    opts = info (helper <*> configParser) $
           fullDesc <> header "jype-html - generating html from jypefiles"
    prelude = either (error "prelude parse error") id $ parseByteString $(embedFile "static/prelude.jype")
    appPrim True decls = primitives ++ prelude ++ decls
    appPrim False decls = primitives ++ decls

generate :: Config -> [Decl] -> IO ()
generate (Config dir _ _) decls = do
    case check decls of
        Left err -> print err
        Right decls' -> do
            createDirectoryIfMissing True dir
            BL.writeFile (dir ++ "/jype.html") $ renderHtml $ html decls'
