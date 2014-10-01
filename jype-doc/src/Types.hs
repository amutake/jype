module Types where

data GenTarget = GenHtml | GenMarkdown deriving (Eq, Show, Read)

data Config = Config
    { configDeployDir :: FilePath
    , configJypeFiles :: FilePath
    , configWithPrelude :: Bool
    , configGenTarget :: GenTarget
    , configOutput :: FilePath
    } deriving (Eq, Show)
