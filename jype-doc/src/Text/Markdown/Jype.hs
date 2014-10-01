{-# LANGUAGE OverloadedStrings #-}

module Text.Markdown.Jype (markdown) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import Data.List (intersperse)
import Data.Maybe (maybeToList)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import Data.Jype.Syntax

markdown :: [Decl] -> ByteString
markdown decls = BB.toLazyByteString $ unlinesB $
    "# jype-doc" : map declSection decls

declSection :: Decl -> Builder
declSection (Decl name body descs) = unlinesB
    [ ""
    , "## " <> BB.stringUtf8 (typeNameConstr name)
    , ""
    , descsSection descs
    , ""
    , bodyTable body
    ]

descsSection :: [Text] -> Builder
descsSection = unlinesB . map (BB.byteString . T.encodeUtf8) . intersperse ""

bodyTable :: Body -> Builder
bodyTable (Object fields) = unlinesB
    [ "**Record Type**"
    , ""
    , "| key | type | description |"
    , "| --- | ---- | ----------- |"
    , unlinesB $ flip map fields $ \f -> mconcat
        [ "| ", BB.stringUtf8 $ fieldKey f, " | "
        , BB.stringUtf8 $ show $ fieldType f, " | "
        , descsSection $ maybeToList (fieldDescription2 f) ++ fieldDescription1 f
        , " |"
        ]
    ]
bodyTable (Choice choices) = unlinesB
    [ "**Union Type**"
    , ""
    , "| type/value | description |"
    , "| ---------- | ----------- |"
    , unlinesB $ flip map choices $ \c -> mconcat
        [ "| ", BB.stringUtf8 $ either show show $ choiceEither c, " | "
        , descsSection $ maybeToList (choiceDescription2 c) ++ choiceDescription1 c
        , " |"
        ]
    ]
bodyTable Primitive = "**Primitive Type**"


unlinesB :: [Builder] -> Builder
unlinesB = mconcat . intersperse "\n"
