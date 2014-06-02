{-# LANGUAGE OverloadedStrings #-}

module Text.Jype.Html
    ( html
    ) where

import Control.Monad
import Data.Foldable (toList)
import Data.Monoid
import Data.List
import Data.Text (Text)

import Text.Blaze.Html5 hiding (html, body, map)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (name)
import qualified Text.Blaze.Html5.Attributes as A

import Data.Jype.Syntax

html :: [Decl] -> Html
html decls = docTypeHtml $ do
    H.head $ do
        link ! rel "stylesheet" ! type_ "text/css" ! href "./jype.css"
    H.body $ do
        h1 "jype-doc"
        mapM_ declHtml decls

declHtml :: Decl -> Html
declHtml (Decl name body descs) = H.div ! A.id (toValue $ typeNameConstr name) ! class_ "decl" $ do
    h2 $ toHtml $ show name
    p $ concatDescs descs
    case body of
        Object fields -> obj fields
        Choice choices -> choice choices
        Primitive -> prim
  where
    obj fields = do
        table $ do
            tr $ do
                th "key"
                th "type"
                th "description"
            forM_ fields $ \field -> tr $ do
                td $ toHtml $ fieldKey field
                td $ typeLink $ fieldType field
                td $ concatDescs $ toList (fieldDescription2 field) ++ fieldDescription1 field
    choice choices = table $ do
        tr $ do
            th "type/value"
            th "description"
        forM_ choices $ \ch -> tr $ do
            td $ toHtml $ either (toHtml . show) typeLink $ choiceEither ch
            td $ concatDescs $ toList (choiceDescription2 ch) ++ choiceDescription1 ch
    prim = p "<primitive>"

typeLink :: ConcreteType -> Html
typeLink (ConcreteType name params) = do
    a ! href (toValue $ "#" ++ name) $ toHtml name
    when (params /= []) $ do
        "["
        sequence_ $ intersperse ", " $ map typeLink params
        "]"

concatDescs :: [Text] -> Html
concatDescs = mconcat . intersperse br . map toHtml
